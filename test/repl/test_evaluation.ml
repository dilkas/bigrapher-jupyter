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

open Format
open OUnit2
open Jupyter.Iopub
open Jupyter.Shell
open Jupyter_repl.Evaluation
open Eval_util

let pp_status ppf status =
  [%to_yojson: Jupyter.Shell.status] status
  |> Yojson.Safe.to_string
  |> pp_print_string ppf

let eval = eval ~ocaml_mode:true

let test__simple_phrase ctxt =
  let actual = eval "let x = (4 + 1) * 3" |> map_content in
  let expected = [Iopub (iopub_success ~count:0 "val x : int = 15\n");
                 Shell (execute_reply ~count:0 SHELL_OK)] in
  assert_equal ~ctxt ~printer:[%show: reply list] expected actual

let test__multiple_phrases ctxt =
  let actual = eval "let x = (4 + 1) * 3\n\
       let y = \"Hello \" ^ \"World\"\n\
       let z = List.map (fun x -> x * 2) [1; 2; 3]\n" |> map_content in
  let expected = [
    Iopub (iopub_success ~count:0 "val x : int = 15\n");
    Iopub (iopub_success ~count:0 "val y : string = \"Hello World\"\n");
    Iopub (iopub_success ~count:0 "val z : int list = [2; 4; 6]\n");
    Shell (execute_reply ~count:0 SHELL_OK)
    ] in
  assert_equal ~ctxt ~printer:[%show: reply list] expected actual

let test__directive ctxt =
  let actual = eval "#load \"str.cma\" ;; Str.regexp \".*\"" |> map_content in
  let expected = [Iopub (iopub_success ~count:0 "- : Str.regexp = <abstr>\n");
                 Shell (execute_reply ~count:0 SHELL_OK)] in
  assert_equal ~ctxt ~printer:[%show: reply list] expected actual

let test__external_command ctxt =
  let actual = eval "Sys.command \"ls -l >/dev/null 2>/dev/null\""
               |> map_content in
  let expected = [Iopub (iopub_success ~count:0 "- : int = 0\n");
                 Shell (execute_reply ~count:0 SHELL_OK)] in
  assert_equal ~ctxt ~printer:[%show: reply list] expected actual

let test__syntax_error ctxt =
  let actual = eval ~count:123 "let let let\nlet" |> map_content in
  let expected =
    [Iopub (error ~value:"compile_error"
              ["\x1b[32mFile \"[123]\", line 1, characters 4-7:\
                \n\x1b[31mError: Syntax error\
                \n\x1b[36m   1: \x1b[30mlet \x1b[4mlet\x1b[0m\x1b[30m let\
                \n\x1b[36m   2: \x1b[30mlet\x1b[0m\n"]);
     Shell (execute_reply ~count:123 SHELL_ERROR)] in
  assert_equal ~ctxt ~printer:[%show: reply list] expected actual

let test__unbound_value ctxt =
  let actual = eval ~count:123 "foo 42" |> map_content in
  let expected =
    [Iopub (error ~value:"compile_error"
              ["\x1b[32mFile \"[123]\", line 1, characters 0-3:\
                \n\x1b[31mError: Unbound value foo\
                \n\x1b[36m   1: \x1b[30m\x1b[4mfoo\x1b[0m\x1b[30m 42\x1b[0m\n"]);
     Shell (execute_reply ~count:123 SHELL_ERROR)] in
  assert_equal ~ctxt ~printer:[%show: reply list] expected actual

let test__type_error ctxt =
  let actual = eval ~count:123 "42 = true" |> map_content in
  let expected =
    [Iopub (error ~value:"compile_error"
              ["\x1b[32mFile \"[123]\", line 1, characters 5-9:\
                \n\x1b[31mError: This expression has type bool but an \
                expression was expected of type\
                \n         int\
                \n\x1b[36m   1: \x1b[30m42 = \x1b[4mtrue\x1b[0m\n"]);
    Shell (execute_reply ~count:123 SHELL_ERROR)] in
  assert_equal ~ctxt ~printer:[%show: reply list] expected actual

let test__long_error_message ctxt =
  let actual = eval ~count:123
      "let a = 42 in\n\
       let b = 43 in\n\
       let c = foo in\n\
       let d = 44 in\n\
       ()" |> map_content in
  let expected =
    [Iopub (error ~value:"compile_error"
              ["\x1b[32mFile \"[123]\", line 3, characters 8-11:\
                \n\x1b[31mError: Unbound value foo\
                \n\x1b[36m   2: \x1b[30mlet b = 43 in\
                \n\x1b[36m   3: \x1b[30mlet c = \x1b[4mfoo\x1b[0m\x1b[30m in\
                \n\x1b[36m   4: \x1b[30mlet d = 44 in\x1b[0m\n"]);
    Shell (execute_reply ~count:123 SHELL_ERROR)] in
  assert_equal ~ctxt ~printer:[%show: reply list] expected actual ;
  let actual = eval ~count:123 "List.\n dummy" |> map_content in
  let expected =
    [Iopub (error ~value:"compile_error"
              ["\x1b[32mFile \"[123]\", line 1, characters 0-12:\
                \n\x1b[31mError: Unbound value List.dummy\
                \n\x1b[36m   1: \x1b[30m\x1b[4mList.\x1b[0m\
                \n\x1b[36m   2: \x1b[30m\x1b[4m dummy\x1b[0m\n"]);
    Shell (execute_reply ~count:123 SHELL_ERROR)] in
  assert_equal ~ctxt ~printer:[%show: reply list] expected actual

let test__exception ctxt =
  let actual = eval "failwith \"FAIL\"" |> map_content in
  let msg =
    if Sys.ocaml_version <= "4.02.3"
    then "\x1b[31mException: Failure \"FAIL\".\n\x1b[0m"
    else "\x1b[31mException: Failure \"FAIL\".\n\
          Raised at file \"pervasives.ml\", line 32, characters 22-33\n\
          Called from file \"toplevel/toploop.ml\", line 180, characters \
          17-56\n\x1b[0m"
  in
  let expected = [Iopub (error ~value:"runtime_error" [msg]);
                 Shell (execute_reply ~count:0 SHELL_ERROR)] in
  assert_equal ~ctxt ~printer:[%show: reply list] expected actual

let test__unknown_directive ctxt =
  let actual = eval "#foo" |> map_content in
  let expected = [Iopub (error ~value:"runtime_error"
                           ["\x1b[31mUnknown directive `foo'.\n\x1b[0m"]);
                 Shell (execute_reply ~count:0 SHELL_ERROR)] in
  assert_equal ~ctxt ~printer:[%show: reply list] expected actual

let test__ppx ctxt =
  let actual = eval "#require \"ppx_deriving.show\" ;; \
                     type t = { x : int } [@@deriving show]" |> map_content in
  let expected =
    [Iopub (iopub_success ~count:0
              "type t = { x : int; }\n\
               val pp : Format.formatter -> t -> Ppx_deriving_runtime.unit = \
               <fun>\n\
               val show : t -> Ppx_deriving_runtime.string = <fun>\n");
    Shell (execute_reply ~count:0 SHELL_OK)] in
  assert_equal ~ctxt ~printer:[%show: reply list] expected actual

let test__camlp4 ctxt =
  let actual = eval_multiple ~ocaml_mode:true ["#camlp4o ;;";
                                               "[< '1 ; '2 >]"] |> map_content in
  let actual1 = List.nth actual 0 in
  let actual2 = List.nth actual 2 in
  let actual3 = List.nth actual 3 in
  let expected1 = Shell (execute_reply ~count:0 SHELL_OK) in
  let expected2 = Iopub (iopub_success ~count:0 "- : int Stream.t = <abstr>\n") in
  let expected3 = Shell (execute_reply ~count:0 SHELL_OK) in
  assert_equal ~ctxt ~printer:[%show: reply] expected1 actual1 ;
  assert_equal ~ctxt ~printer:[%show: reply] expected2 actual2 ;
  assert_equal ~ctxt ~printer:[%show: reply] expected3 actual3

let test__incomplete_model ctxt =
  let actual = Eval_util.eval
      "ctrl Foo = 0;\
       \nbig foo = Foo.1;" |> map_content in
  let actual1 = List.nth actual 0 in
  let actual2 = List.nth actual 2 in
  let expected1 = Iopub (iopub_success ~count:0 "foo") in
  let expected2 = Shell (execute_reply ~count:0 SHELL_OK) in
  assert_equal ~ctxt ~printer:[%show: reply] expected1 actual1 ;
  assert_equal ~ctxt ~printer:[%show: reply] expected2 actual2

let test__ocaml_from_bigrapher ctxt =
  let actual = Eval_util.eval "%ocaml\
                               \n1" |> map_content in
  let expected = [Iopub (iopub_success ~count:0 "- : int = 1\n");
                  Shell (execute_reply ~count:0 SHELL_OK)] in
  assert_equal ~ctxt ~printer:[%show: reply list] expected actual

let test__bigrapher_error ctxt =
  let actual = Eval_util.eval "!" |> map_content in
  let expected = [Iopub (stream ~name:IOPUB_STDERR "File '[0].big', line 1, \
                                                    characters 0-1\n");
                  Iopub (stream ~name:IOPUB_STDERR
                           "\x1b[01m\x1b[31mError\x1b[m\x1b[m: \
                            Unknown character `!'\n");
                  Shell (execute_reply ~count:0 SHELL_ERROR)] in
  assert_equal ~ctxt ~printer:[%show: reply list] expected actual

let test__api_model ctxt =
  let actual = eval "#use \"topfind\" ;;\
                     \n#require \"bigraph\" ;;\
                     \nopen Bigraph\
                     \nlet () =\
                     \nlet foo = Big.one in\
                     \nprint_endline (Big.to_string foo)" |> map_content in
  let expected = [Iopub (stream ~name:IOPUB_STDOUT "- : unit = ()\n");
                  Iopub (stream ~name:IOPUB_STDOUT "- : unit = ()\n");
                  Iopub (stream ~name:IOPUB_STDOUT "{}\n");
                  Iopub (stream ~name:IOPUB_STDOUT "1 0 0\n");
                  Shell (execute_reply ~count:0 SHELL_OK)] in
  assert_equal ~ctxt ~printer:[%show: reply list] expected actual

let test__stochastic_model ctxt =
  let actual = Eval_util.eval "ctrl Foo = 0;\
                               \nbig foo = Foo.1;\
                               \n # react as part of a comment\
                               \nreact bar = foo -[0.5]-> foo;\
                               \nreact baz = foo --> foo;\
                               \nbegin sbrs\
                               \n  init foo;\
                               \n  rules = [{bar}];\
                               \n  preds = {foo};\
                               \nend" |> map_content in
  let actual1 = List.nth actual 0 in
  let actual2 = List.nth actual 2 in
  let actual3 = List.nth actual 4 in
  let expected1 = Iopub (iopub_success ~count:0 "foo") in
  let expected2 = Iopub (iopub_success ~count:0 "bar") in
  let expected3 = Shell (execute_reply ~count:0 SHELL_OK) in
  assert_equal ~ctxt ~printer:[%show: reply] expected1 actual1 ;
  assert_equal ~ctxt ~printer:[%show: reply] expected2 actual2 ;
  assert_equal ~ctxt ~printer:[%show: reply] expected3 actual3

let test__clear_buffer ctxt =
  let actual = Eval_util.eval_multiple ["ctrl Foo = 0;"; "%clear\
                                                          \nbig foo = Foo.1;"]
               |> map_content in
  let expected = [Shell (execute_reply ~count:0 SHELL_OK);
                  Iopub (stream ~name:IOPUB_STDERR
                           "File '[0].big', line 1, characters 10-13\n");
                  Iopub (stream ~name:IOPUB_STDERR
                           "\x1b[01m\x1b[31mError\x1b[m\x1b[m: Unbound \
                            variable Foo\n");
                  Shell (execute_reply ~count:0 SHELL_ERROR)] in
  assert_equal ~ctxt ~printer:[%show: reply list] expected actual

let suite =
  "Evaluation" >::: [
    "eval" >::: [
      "simple_phrase" >:: test__simple_phrase;
      "multiple_phrases" >:: test__multiple_phrases;
      "directive" >:: test__directive;
      "external_command" >:: test__external_command;
      "syntax_error" >:: test__syntax_error;
      "unbound_value" >:: test__unbound_value;
      "type_error" >:: test__type_error;
      "long_error_message" >:: test__long_error_message;
      "exception" >:: test__exception;
      "unknown_directive" >:: test__unknown_directive;
      "ppx" >:: test__ppx;
      "camlp4" >:: test__camlp4;
      "incomplete_model" >:: test__incomplete_model;
      "ocaml_from_bigrapher" >:: test__ocaml_from_bigrapher;
      "bigrapher_error" >:: test__bigrapher_error;
      "api_model" >:: test__api_model;
      "stochastic_model" >:: test__stochastic_model;
      "clear_buffer" >:: test__clear_buffer;
    ]
  ]

let () =
  init ~init_file:"fixtures/ocamlinit.ml" () ;
  run_test_tt_main suite
