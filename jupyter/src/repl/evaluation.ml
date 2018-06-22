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

let eval_ocaml ?(error_ctx_size = 1) ~send ~count code =
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

(* Does this list of lines contain a begin-end block? *)
let has_main_block lines =
  List.exists (fun line -> String.length line > 5 &&
                           Str.first_chars line 5 = "begin") lines

(* Return the second word in the string, where keyword_length denotes the
   length of the first word *)
let extract_name keyword_length line =
  let end_of_name = String.index_from line (keyword_length + 1) ' ' in
  String.sub line (keyword_length + 1) (end_of_name - keyword_length - 1)

(* For every row starting with the given keyword, return the second word *)
let list_defined_entities keyword lines =
  let keyword_length = String.length keyword in
  let relevant_lines = List.filter (fun line ->
      String.length line > keyword_length &&
      Str.first_chars line keyword_length = keyword) lines in
  List.map (extract_name keyword_length) relevant_lines

(* If we find a line starting with 'react' with the name not in rules_to_keep,
   remove everything up to the next semicolon *)
let rec remove_reaction_rules_from_previous_cells rules_to_keep text =
  let regular_expression = Str.regexp_string "react" in
  try
    let i = Str.search_forward regular_expression text 0 in
    if i = 0 || text.[i - 1] = '\n' then
      let name = extract_name (i + 5) text in
      (* if the name of the reaction rule is in our rules_to_keep list, then
         everything below it will be as well *)
      if List.mem name rules_to_keep then text
      else
        let end_of_definition = String.index_from text i ';' in
        let remaining_text = Str.string_after text (end_of_definition + 1) in
        let text_before_reaction_rule = Str.string_before text i in
        text_before_reaction_rule ^
        remove_reaction_rules_from_previous_cells rules_to_keep remaining_text
    else
      let text_before_react = Str.string_before text i in
      let text_after_react = Str.string_after text (i + 5) in
      text_before_react ^
      remove_reaction_rules_from_previous_cells rules_to_keep text_after_react
  with Not_found -> text

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
let complete_model bigraphs rules model =
  let taken_names = bigraphs @ rules in
  let random_big = generate_string_not_in taken_names "" in
  let random_ctrl = generate_string_not_in taken_names "B" in
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

(* Run bigrapher on the given model, saving generated diagrams in
   diagram_directory *)
let run_bigrapher diagram_directory model_filename =
  let command= Printf.sprintf "bigrapher validate -d %s -f svg %s"
      diagram_directory model_filename in
  let channel = Unix.open_process_in command in
  let output_buffer = Buffer.create 256 in
  begin
    try
      while true do
        Buffer.add_channel output_buffer channel 1
      done
    with End_of_file -> ()
  end ;
  channel, output_buffer

(* Extract code from the buffer, modifying it as necessary *)
let code_of_buffer model_is_full bigraphs reaction_rules =
  if model_is_full then Buffer.contents bigrapher_buffer
  else
    let all_code = Buffer.contents bigrapher_buffer in
    let filtered_code = remove_reaction_rules_from_previous_cells reaction_rules
        all_code in
    complete_model bigraphs reaction_rules filtered_code

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

(* Evaluate a given code block, sending/displaying any results and returning a
   success/failure status. Send - the function used for sending textual output.
   Count - the number of the cell according to the run order (starting from 0). *)
let eval ?(error_ctx_size = 1) ~send ~count code =
  if String.sub code 0 7 = "%ocaml\n"
  then
    let remaining_code = Str.string_after code 7 in
    eval_ocaml ~error_ctx_size ~send ~count remaining_code
  else
    let dirname = prepare_directory_for_cell "jupyter-images" 0o700 count in
    let lines = String.split_on_char '\n' code in

    let model_is_full = has_main_block lines in
    let bigraphs = list_defined_entities "big" lines in
    let reaction_rules = list_defined_entities "react" lines in

    Buffer.add_string bigrapher_buffer (code ^ "\n") ;
    let contents = code_of_buffer model_is_full bigraphs reaction_rules in

    let filename = write_code_to_file count contents in
    let channel, output_buffer = run_bigrapher dirname filename in
    (*send (iopub_success ~count (Buffer.contents output_buffer)) ;*)

    match Unix.close_process_in channel with
    | Unix.WEXITED 0 ->
      display_bigraphs send count dirname bigraphs ;
      display_reaction_rules send count dirname reaction_rules ;
      if model_is_full then truncate_buffer code ;
      safe_remove filename ;
      Shell.SHELL_OK
    | _ ->
      truncate_buffer code ;
      safe_remove filename ;
      Shell.SHELL_ERROR
