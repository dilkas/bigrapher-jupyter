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
open Jupyter.Message
open Jupyter.Iopub
open Jupyter.Shell
open Jupyter_repl.Evaluation
open Eval_util

let eval = eval ~ocaml_mode:true

let test__simple_phrase ctxt =
  let actual = eval "let x = (4 + 1) * 3" |> map_content in
  let expected = [
    Iopub (iopub_success ~count:0 "val x : int = 15\n");
    Shell (execute_reply ~count:0 SHELL_OK);
  ] in
  assert_equal ~ctxt ~printer:[%show: reply list] expected actual

let test__capture_stdout ctxt =
  let actual =
    eval "print_endline \"Hello World\""
    |> map_content
    |> List.sort compare in (* the order of elements is NOT important *)
  let expected = [
    Iopub (stream ~name:IOPUB_STDOUT "Hello World\n");
    Iopub (iopub_success ~count:0 "- : unit = ()\n");
    Shell (execute_reply ~count:0 SHELL_OK);
  ] in
  assert_equal ~ctxt ~printer:[%show: reply list] expected actual

let test__capture_stderr ctxt =
  let actual =
    eval "prerr_endline \"Hello World\""
    |> map_content
    |> List.sort compare in (* the order of elements is NOT important *)
  let expected = [
    Iopub (stream ~name:IOPUB_STDERR "Hello World\n");
    Iopub (iopub_success ~count:0 "- : unit = ()\n");
    Shell (execute_reply ~count:0 SHELL_OK);
  ] in
  assert_equal ~ctxt ~printer:[%show: reply list] expected actual

(** Check [!Sys.interactive] is [true]. *)
let test__sys_interactive ctxt =
  let actual = eval "!Sys.interactive" |> map_content in
  let expected = [
    Iopub (iopub_success ~count:0 "- : bool = true\n");
    Shell (execute_reply ~count:0 SHELL_OK);
  ] in
  assert_equal ~ctxt ~printer:[%show: reply list] expected actual

let test__simple_model ctxt =
  let actual = Eval_util.eval ~ocaml_mode:false
      "ctrl Foo = 0;
big foo = Foo.1;
react bar = foo --> foo;
begin brs
  init foo;
  rules = [{bar}];
  preds = {foo};
end" |> map_content in
  let actual1 = List.nth actual 0 in
  let actual2 = List.nth actual 2 in
  let actual3 = List.nth actual 4 in
  let expected1 = Iopub (iopub_success ~count:0 "foo") in
  let expected2 = Iopub (iopub_success ~count:0 "bar") in
  let expected3 = Shell (execute_reply ~count:0 SHELL_OK) in
  assert_equal ~ctxt ~printer:[%show: reply] expected1 actual1 ;
  assert_equal ~ctxt ~printer:[%show: reply] expected2 actual2 ;
  assert_equal ~ctxt ~printer:[%show: reply] expected3 actual3

let suite =
  "Process" >::: [
    "simple_phrase" >:: test__simple_phrase;
    "capture_stdout" >:: test__capture_stdout;
    "capture_stderr" >:: test__capture_stderr;
    "sys_interactive" >:: test__sys_interactive;
    "simple_model" >:: test__simple_model;
  ]

let () = run_test_tt_main suite
