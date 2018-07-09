(* ocaml-jupyter --- An OCaml kernel for Jupyter

   Copyright (c) 2017 Akinori ABE

   Permission is hereby granted, free of charge, to any person obtaining a copy
   of this software and associated documentation files (the "Software"), to deal
   in the Software without restriction, including without limitation the rights
   to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
   copies of the Software, and to permit persons to whom the Software is
   furnished to do so, subject to the following conditions:

   The above copyright notice and this permission notice shall be included in all
   copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
   IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
   FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
   AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
   LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
   OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
   SOFTWARE. *)

(** Top-level loop of OCaml code evaluation *)

open Format
open Jupyter
open Utils

type model = Incomplete | BRS | StochasticBRS
type subcommand = Full | Sim of int | Validate

let ocaml_buffer = Buffer.create 256
let bigrapher_buffer = Buffer.create 256
let ppf = formatter_of_buffer ocaml_buffer

(** {2 Initialization} *)

let readenv ppf =
  let open Compenv in
  readenv ppf Before_args ;
  readenv ppf Before_link

let prepare () =
  Toploop.set_paths() ;
  !Toploop.toplevel_startup_hook () ;
  Topdirs.dir_cd (Sys.getcwd ()) (* required for side-effect initialization in Topdirs *)

let replace_out_phrase () =
  let re = Str.regexp ".*ocamltoplevel\\.cma.*" in
  let old_out_phrase = !Oprint.out_phrase in
  let print_out_phrase ppf = function
    (* Ignore the exception raised at ocaml/toplevel/toploop.ml *)
    | Outcometree.Ophr_exception (Invalid_argument msg, _)
      when Str.string_match re msg 0 -> ()
    | ophr -> old_out_phrase ppf ophr
  in
  Oprint.out_phrase := print_out_phrase

let init_toploop () =
  try
    Toploop.initialize_toplevel_env ()
  with Env.Error _ | Typetexp.Error _ as exn ->
    Location.report_exception ppf exn ;
    exit 2

let load_ocamlinit = function
  | None -> ()
  | Some path ->
    if Sys.file_exists path
    then ignore (Toploop.use_silently std_formatter path)
    else eprintf "Init file not found: \"%s\".@." path

let init ?(preload = ["stdlib.cma"]) ?(preinit = ignore) ?init_file () =
  let ppf = Format.err_formatter in
  Clflags.debug := true ;
  Location.formatter_for_warnings := ppf ;
  Sys.catch_break true ;
  replace_out_phrase () ;
  readenv ppf ;
  prepare () ;
  init_toploop () ;
  List.iter (Topdirs.dir_load ppf) preload ;
  preinit () ;
  load_ocamlinit init_file

let setvalue name value = Toploop.setvalue name (Obj.repr value)

(** {2 Communication} *)

let iopub_success ?metadata ~count msg =
  Iopub.execute_result ?metadata ~count (`Assoc ["text/plain", `String msg])

let iopub_interrupt () =
  Iopub.error ~name:"interrupt" ~value:"intterupt" [
    sprintf "%sException: Sys.Break.%s"
      AnsiCode.FG.red AnsiCode.reset
  ]

(** {2 Execution} *)

let eval_phrase ~filename phrase =
  Compat.reset_fatal_warnings () ;
  let phrase' = Compat.preprocess_phrase ~filename phrase in (* apply PPX *)
  Env.reset_cache_toplevel () ;
  let is_ok = Toploop.execute_phrase true ppf phrase' in
  let message = Buffer.contents ocaml_buffer in
  Buffer.clear ocaml_buffer ;
  (is_ok, message)

let eval_ocaml ?(_produce_output=false) ?(error_ctx_size = 1) ~send ~count code =
  let filename = sprintf "[%d]" count in
  let rec loop status = function
    | [] -> status
    | phrase :: tl ->
      match eval_phrase ~filename phrase with
      | true, "" -> loop status tl
      | true, msg -> send (iopub_success ~count msg) ; loop status tl
      | false, msg ->
        send (Iopub.error ~value:"runtime_error"
                [AnsiCode.FG.red ^ msg ^ AnsiCode.reset]) ;
        Shell.SHELL_ERROR
  in
  try
    let lexbuf = Lexing.from_string (code ^ "\n") in
    Location.init lexbuf filename ;
    Location.input_name := filename ;
    Location.input_lexbuf := Some lexbuf ;
    loop Shell.SHELL_OK (!Toploop.parse_use_file lexbuf)
  with
  | Sys.Break ->
    send (iopub_interrupt ()) ;
    Shell.SHELL_ABORT
  | exn ->
    let msg = Error.to_string_hum ~ctx_size:error_ctx_size exn in
    send (Iopub.error ~value:"compile_error" [msg]) ;
    Shell.SHELL_ERROR

(* Display each reaction rule in names_of_rules by printing its name and
   creating an HTML table, connecting both diagrams *)
let display_reaction_rules send count dirname names_of_rules =
  List.iter (fun name ->
      send (iopub_success ~count name) ;
      let partial_filename = Filename.concat dirname name in
      ignore (Jupyter_notebook.display "text/html" (Printf.sprintf "
<table>
    <tr>
        <td><img src=\"%s_lhs.svg\"></td>
        <td style=\"padding-left: 25px; padding-right: 25px;\">
            <h1>&rarr;</h1>
        </td>
        <td><img src=\"%s_rhs.svg\"></td>
    </tr>
</table>
" partial_filename partial_filename))
    ) names_of_rules

(* Display bigraphs listed in names *)
let display_bigraphs send count dirname names =
  List.iter (fun name ->
      send (iopub_success ~count name) ;
      ignore (Jupyter_notebook.display_file "image/svg+xml"
                ((Filename.concat dirname name) ^ ".svg"))
    ) names

(* Return a list of files in a directory (with their relative paths from the
   current directory) *)
let files_in_dir dirname =
  let files = Sys.readdir dirname in
  let full_filenames = Array.map (Filename.concat dirname) files in
  Array.to_list full_filenames

(* Return the second word in the string, where keyword_length denotes the
   length of the first word *)
let extract_name keyword_length line =
  let end_of_name =
    try String.index_from line (keyword_length + 1) ' '
    with Not_found -> String.length line
  in
  String.sub line (keyword_length + 1) (end_of_name - keyword_length - 1)

(* Find the type of the model from a list of lines (including the 'incomplete'
   option) *)
let rec get_model_type = function
  | [] -> Incomplete
  | line :: remaining_lines ->
    if String.length line > 5 && Str.first_chars line 5 = "begin" then
      if extract_name 5 line = "sbrs" then StochasticBRS else BRS
    else get_model_type remaining_lines

(* For every row starting with the given keyword, return the second word *)
let list_defined_entities keyword lines =
  let keyword_length = String.length keyword in
  let relevant_lines = List.filter (fun line ->
      String.length line > keyword_length &&
      Str.first_chars line keyword_length = keyword) lines in
  List.map (extract_name keyword_length) relevant_lines

let all_defined_entities lines =
  List.fold_left (fun entities keyword ->
      entities @ list_defined_entities keyword lines) []
    ["float"; "int"; "ctrl"; "atomic ctrl"; "big"; "react"]

(* Is the reaction rule in text starting at first_index a stochastic rule? *)
let is_stochastic text first_index =
  let arrow = Str.regexp_string "->" in
  let arrow_index = Str.search_forward arrow text first_index in
  text.[arrow_index - 1] = ']'

let rec remove_non_stochastic_rules text = function
  | [] -> [], text
  | (rule :: other_rules) as rules ->
    let regular_expression = Str.regexp_string "react" in
    let i = Str.search_forward regular_expression text 0 in
    if i = 0 || text.[i - 1] = '\n' then
      if is_stochastic text i then
        (* keep the rule *)
        let second_letter_index = i + 1 in
        let text_before_react = Str.string_before text second_letter_index in
        let text_after_react = Str.string_after text second_letter_index in
        let (remaining_rules, remaining_filtered_text) =
          remove_non_stochastic_rules text_after_react other_rules in
        rule :: remaining_rules, text_before_react ^ remaining_filtered_text
      else
        (* remove the rule *)
        let end_of_definition = String.index_from text i ';' in
        let remaining_text = Str.string_after text (end_of_definition + 1) in
        let text_before_reaction_rule = Str.string_before text i in
        let (remaining_rules, remaining_filtered_text) =
          remove_non_stochastic_rules remaining_text other_rules in
        remaining_rules, text_before_reaction_rule ^ remaining_filtered_text
    else
      (* just keep looking... *)
      let second_letter_index = i + 1 in
      let text_before_react = Str.string_before text second_letter_index in
      let text_after_react = Str.string_after text second_letter_index in
      let (remaining_rules, remaining_filtered_text) =
        remove_non_stochastic_rules text_after_react rules in
      remaining_rules, text_before_react ^ remaining_filtered_text

(* Generates a random (lowercase) string not in the given list. Candidate is
   the initial attempt, can be empty. *)
let rec generate_string_not_in list candidate =
  if candidate = "" || List.mem candidate list then
    let random_letter_int = int_of_char 'a' + Random.int 26 in
    let random_letter = char_of_int random_letter_int in
    let random_letter_string = String.make 1 random_letter in
    let new_candidate = candidate ^ random_letter_string in
    generate_string_not_in list new_candidate
  else candidate

(* Add a default begin-end block to an incomplete model *)
let complete_model taken_names model =
  let random_big = generate_string_not_in taken_names "" in
  let random_ctrl = generate_string_not_in taken_names "C" in
  let random_react = generate_string_not_in (random_big :: taken_names) "" in
  let begin_end_block = Printf.sprintf
      "\n
ctrl %s = 0;
big %s = %s.1;
react %s = %s --> %s;
begin brs
  init %s;
  rules = [{%s}];
  preds = {%s};
end\n"
      random_ctrl random_big random_ctrl random_react random_big random_big
      random_big random_react random_big in
  model ^ begin_end_block

(* Takes two lists of the names of bigraphs and reaction rules that have
   already been defined and a model type. Adds a begin-end block if it is
   missing. Removes non-stochastic reaction rules if we're given a stochastic
   model. *)
let code_of_buffer taken_names reaction_rules = function
  | BRS -> reaction_rules, Buffer.contents bigrapher_buffer
  | StochasticBRS ->
    let code = Buffer.contents bigrapher_buffer in
    remove_non_stochastic_rules code reaction_rules
  | Incomplete ->
    let code = Buffer.contents bigrapher_buffer in
    reaction_rules, complete_model taken_names code

(* If the directory exists, remove all files in it; if not, create it *)
let prepare_directory_for_cell image_directory permissions count =
  (* create a directory for all images if needed *)
  if not (Sys.file_exists image_directory) then
    Unix.mkdir image_directory permissions ;

  let count_str = string_of_int count in
  let dirname = Filename.concat image_directory count_str in
  begin
    try Unix.mkdir dirname permissions
    with _ -> List.iter Sys.remove (files_in_dir dirname)
  end ;
  dirname

(* Remove code from the last cell from the buffer *)
let truncate_buffer code =
  (* -1 because we added a newline *)
  let new_length = Buffer.length bigrapher_buffer - String.length code - 1 in
  Buffer.truncate bigrapher_buffer new_length

let write_code_to_file count contents =
  let filename = Printf.sprintf "[%d].big" count in
  let channel = open_out filename in
  Printf.fprintf channel "%s" contents ;
  close_out channel ;
  filename

let safe_remove filename =
  try Sys.remove filename
  with _ -> ()

(* Does the text start with the pattern (a smaller string)? *)
let starts_with text pattern =
  let pattern_length = String.length pattern in
  String.length text >= pattern_length &&
  String.sub text 0 pattern_length = pattern

let run_bigrapher ?(_produce_output = false) ~mode ~send ~count code =
  let dirname = prepare_directory_for_cell "jupyter-images" 0o700 count in
  let lines = String.split_on_char '\n' code in

  if mode <> Validate && get_model_type lines = Incomplete then
    begin
      send (Iopub.error ~value:"runtime_error"
              ["Incomplete models cannot be run \
                in %states or %simulate mode"]) ;
      Shell.SHELL_ERROR
    end
  else
    let model_type = get_model_type lines in
    let bigraphs = list_defined_entities "big" lines in
    let unfiltered_reaction_rules = list_defined_entities "react" lines in
    let taken_names = all_defined_entities lines in

    Buffer.add_string bigrapher_buffer (code ^ "\n") ;
    let (reaction_rules, contents) = code_of_buffer taken_names
        unfiltered_reaction_rules model_type in

    let code_filename = write_code_to_file count contents in
    let filename = Printf.sprintf "[%d]" count in
    let image_filename = Filename.concat dirname filename in
    let command = match mode with
      | Full -> Printf.sprintf "bigrapher full -t %s -f svg -s ./ %s"
                  image_filename code_filename
      | Sim n -> Printf.sprintf "bigrapher sim -t %s -f svg -S %d %s"
                   image_filename n code_filename
      | Validate -> Printf.sprintf "bigrapher validate -d %s -f svg %s"
                      dirname code_filename 
    in
    let channel, output_buffer = capture_output command in
    if _produce_output then
      send (iopub_success ~count (Buffer.contents output_buffer)) ;

    match Unix.close_process_in channel with
    | Unix.WEXITED 0 ->
      begin
        match mode with
        | Full ->
          image_filename ^ ".svg"
          |> Jupyter_notebook.display_file "image/svg+xml" 
          |> ignore
        | Sim _ -> display_bigraphs send count dirname bigraphs
        | Validate ->
          display_bigraphs send count dirname bigraphs ;
          display_reaction_rules send count dirname reaction_rules
      end ;
      if model_type <> Incomplete then truncate_buffer code ;
      safe_remove code_filename ;
      Shell.SHELL_OK
    | _ ->
      truncate_buffer code ;
      safe_remove code_filename ;
      Shell.SHELL_ERROR

(* Evaluate a given code block, sending/displaying any results and returning a
   success/failure status. Send - the function used for sending textual output.
   Count - the number of the cell according to the run order
   (starting from 0). *)
let rec eval ?(_produce_output = false) ?(error_ctx_size = 1)
    ~send ~count code =
  if starts_with code "%clear\n" then
    begin
      Buffer.clear bigrapher_buffer ;
      let remaining_code = Str.string_after code 7 in
      eval ~error_ctx_size ~send ~count remaining_code
    end
  else if starts_with code "%ocaml\n" then
    let remaining_code = Str.string_after code 7 in
    eval_ocaml ~error_ctx_size ~send ~count remaining_code
  else if starts_with code "%output\n" then
    let remaining_code = Str.string_after code 8 in
    eval ~_produce_output:true ~error_ctx_size ~send ~count remaining_code
  else if starts_with code "%states\n" then
    let remaining_code = Str.string_after code 8 in
    run_bigrapher ~_produce_output ~mode:Full ~send ~count remaining_code
  else if starts_with code "%simulate" then
    run_bigrapher ~_produce_output ~mode:(Sim 1) ~send ~count code
  else
    run_bigrapher ~_produce_output ~mode:Validate ~send ~count code