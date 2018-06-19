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

let buffer = Buffer.create 256
let ppf = formatter_of_buffer buffer

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
  let message = Buffer.contents buffer in
  Buffer.clear buffer ;
  (is_ok, message)

let display_rules send count directory names =
  List.iter (fun name ->
      send (iopub_success ~count name) ;
      let partial_filename = Filename.concat directory name in
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
    ) names

let display_bigraphs send count directory names =
  List.iter (fun name ->
      send (iopub_success ~count name) ;
      ignore (Jupyter_notebook.display_file "image/svg+xml"
                ((Filename.concat directory name) ^ ".svg"))
    ) names

let files_in_dir dirname =
  Array.to_list (Array.map (Filename.concat dirname) (Sys.readdir dirname))

let has_main_block lines =
  List.exists (fun line -> String.length line > 5 &&
                           Str.first_chars line 5 = "begin") lines

let extract_name keyword_length line =
  let end_of_name = String.index_from line (keyword_length + 1) ' ' in
  String.sub line (keyword_length + 1) (end_of_name - keyword_length - 1)

let list_defined_entities keyword lines =
  let keyword_length = String.length keyword in
  let relevant_lines = List.filter (fun line ->
      String.length line > keyword_length &&
      Str.first_chars line keyword_length = keyword) lines in
  List.map (extract_name keyword_length) relevant_lines

(* if we find a 'react' followed by a name not in 'rules', delete everything
   until we reach a semicolon *)
let rec remove_unnecessary_rules rules text =
  let re = Str.regexp_string "react" in
  try
    let i = Str.search_forward re text 0 in
    if i = 0 || text.[i - 1] = '\n' then
      let name = extract_name (i + 5) text in
      (* if the name of the reaction rule is in our "to keep" list, then
         everything below it will be as well *)
      if List.mem name rules then text
      else
        let end_of_definition = String.index_from text i ';' in
        let remaining_text = Str.string_after text (end_of_definition + 1) in
        let text_before_rule = Str.string_before text i in
        text_before_rule ^ remove_unnecessary_rules rules remaining_text
    else
      let text_before_react = Str.string_before text i in
      let text_after_react = Str.string_after text (i + 5) in
      text_before_react ^ remove_unnecessary_rules rules text_after_react
  with Not_found -> text

let rec generate_string_not_in list candidate =
  if candidate = "" || List.mem candidate list then
    generate_string_not_in list
      (candidate ^ (String.make 1
                      (char_of_int (int_of_char 'a' + Random.int 26))))
  else candidate

(* adds the missing parts to make a complete model *)
let complete_model bigraphs rules model =
  let taken_names = bigraphs @ rules in
  let random_big = generate_string_not_in taken_names "" in
  let random_ctrl = generate_string_not_in taken_names "B" in
  let random_react = generate_string_not_in (random_big :: taken_names) "" in
  let model = remove_unnecessary_rules rules model in
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

(* TODO: delete created files *)
(* TODO: don't everything after running a complete model *)
(* TODO: split into multiple functions *)
(* TODO: more comments *)
let eval ?(error_ctx_size = 1) ~send ~count code =
  (* manage the image directory *)
  let dirname = Printf.sprintf "img-%d" count in
  begin
    try
      Unix.mkdir dirname 0o700
    with _ -> List.iter Sys.remove (files_in_dir dirname)
  end ;

  let lines = String.split_on_char '\n' code in
  let full_model = has_main_block lines in
  let bigraphs = list_defined_entities "big" lines in
  let rules = list_defined_entities "react" lines in
  Buffer.add_string buffer code ;
  Buffer.add_char buffer '\n' ;

  let contents = if full_model then Buffer.contents buffer
    else complete_model bigraphs rules (Buffer.contents buffer) in

  (* write code to a file *)
  let filename = sprintf "[%d].big" count in
  let oc = open_out filename in
  Printf.fprintf oc "%s" contents ;
  close_out oc ;

  (* run bigrapher *)
  let channel = Unix.open_process_in
      (Printf.sprintf "bigrapher validate -d %s -f svg %s" dirname filename) in
  let output_buffer = Buffer.create 256 in
  begin
    try
      while true do
        Buffer.add_channel output_buffer channel 1
      done
    with End_of_file -> ()
  end ;

  (*send (iopub_success ~count (Buffer.contents output_buffer)) ;*)

  match Unix.close_process_in channel with
  | Unix.WEXITED 0 ->
    display_bigraphs send count dirname bigraphs ;
    display_rules send count dirname rules ;
    if full_model then Buffer.clear buffer ;
    Sys.remove filename ;
    Shell.SHELL_OK
  | _ ->
    (* -1 because we added a newline *)
    Buffer.truncate buffer (Buffer.length buffer - String.length code - 1) ;
    Sys.remove filename ;
    Shell.SHELL_ERROR

  (*let rec loop status = function
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
  *)
