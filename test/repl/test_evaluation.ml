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

let test__bigraphers_output ctxt =
  let actual = Eval_util.eval "%output\
                               \nctrl Foo = 0;\
                               \nbig foo = Foo.1;\
                               \nreact bar = foo --> foo;\
                               \nbegin brs\
                               \n  init foo;\
                               \n  rules = [{bar}];\
                               \n  preds = {foo};\
                               \nend" |> map_content
               |> List.length in
  assert_equal ~ctxt ~printer:[%show: int] 6 actual

let test__probabilistic_model_simulation ctxt =
  let actual = Eval_util.eval "%simulate 0\
                               \natomic ctrl M = 1;\
                               \nctrl R = 0;\
                               \nctrl B = 0;\
                               \nctrl Out = 1;\
                               \nbig s0 =\
                               \nB.(R.M{x} | R.M{x} | R.1 | M{x}) || Out{y}.1;\
                               \nfloat p_enter = 0.2;\
                               \nreact enter_room =\
                               \nR | M{x} -[p_enter]-> R.(M{x} | id);\
                               \nreact leave_room =\
                               \nR.(M{x} | id) -[1. - p_enter]-> R | M{x};\
                               \nreact leave_building =\
                               \nB.(M{x} | id) || Out{y} -[0.3]-> \
                               B | {x} || Out{y}.(id | M{y});\
                               \nbegin pbrs\
                               \ninit s0;\
                               \nrules = [ { enter_room, leave_room, \
                               \nleave_building } ];\
                               end" |> map_content in
  let actual1 = List.nth actual 1 in
  let actual_length = List.length actual in
  let expected = Shell (execute_reply ~count:0 SHELL_OK) in
  assert_equal ~ctxt ~printer:[%show: int] 2 actual_length ;
  assert_equal ~ctxt ~printer:[%show: reply] expected actual1

let test__incomplete_model_state_diagram ctxt =
  let actual = Eval_util.eval "%states\n" |> map_content in
  let expected = [Iopub (error ~value:"runtime_error"
                           ["You need a begin-end block in order to generate \
                             a state diagram"]);
                  Shell (execute_reply ~count:0 SHELL_ERROR)] in
  assert_equal ~ctxt ~printer:[%show: reply list] expected actual

let test__incomplete_model_simulation ctxt =
  let actual = Eval_util.eval "%simulate 0\n" |> map_content in
  let expected = [Iopub (error ~value:"runtime_error"
                           ["You need a begin-end block in order to run a \
                             simulation"]);
                  Shell (execute_reply ~count:0 SHELL_ERROR)] in
  assert_equal ~ctxt ~printer:[%show: reply list] expected actual

let test__stochastic_model_simulation ctxt =
  let actual = Eval_util.eval "%simulate 0\
                               \nctrl Foo = 0;\
                               \nbig foo = Foo.1;\
                               \nreact bar = foo -[0.5]-> foo;\
                               \nbegin sbrs\
                               \n  init foo;\
                               \n  rules = [{bar}];\
                               \n  preds = {foo};\
                               \nend" |> map_content in
  let actual1 = List.nth actual 1 in
  let actual_length = List.length actual in
  let expected = Shell (execute_reply ~count:0 SHELL_OK) in
  assert_equal ~ctxt ~printer:[%show: int] 2 actual_length ;
  assert_equal ~ctxt ~printer:[%show: reply] expected actual1

let test__state_diagram ctxt =
  let actual = Eval_util.eval "%states\
                               \nctrl Foo = 0;\
                               \nbig foo = Foo.1;\
                               \nreact bar = foo --> foo;\
                               \nbegin brs\
                               \n  init foo;\
                               \n  rules = [{bar}];\
                               \n  preds = {foo};\
                               \nend" |> map_content in
  let actual1 = List.nth actual 1 in
  let actual_length = List.length actual in
  let expected = Shell (execute_reply ~count:0 SHELL_OK) in
  assert_equal ~ctxt ~printer:[%show: int] 2 actual_length ;
  assert_equal ~ctxt ~printer:[%show: reply] expected actual1

let test__stochastic_simulation_with_bad_argument ctxt =
  let actual = Eval_util.eval "%simulate a
                               \nctrl Foo = 0;\
                               \nbig foo = Foo.1;\
                               \nreact bar = foo -[0.5]-> foo;\
                               \nbegin sbrs\
                               \n  init foo;\
                               \n  rules = [{bar}];\
                               \n  preds = {foo};\
                               \nend" |> map_content in
  let expected = [Iopub (error ~value:"runtime_error"
                           ["For a stochastic system, %simulate should be \
                             followed by the maximum simulation time (as a \
                             floating-point number)"]);
                  Shell (execute_reply ~count:0 SHELL_ERROR)] in
  assert_equal ~ctxt ~printer:[%show: reply list] expected actual

let test__deterministic_simulation_with_bad_argument ctxt =
  let actual = Eval_util.eval "%simulate a
                               \nctrl Foo = 0;\
                               \nbig foo = Foo.1;\
                               \nreact bar = foo --> foo;\
                               \nbegin brs\
                               \n  init foo;\
                               \n  rules = [{bar}];\
                               \n  preds = {foo};\
                               \nend" |> map_content in
  let expected = [Iopub (error ~value:"runtime_error"
                           ["For a non-stochastic model, %simulate should be \
                           followed by the maximum number of simulation steps \
                           (as a non-negative integer)"]);
                  Shell (execute_reply ~count:0 SHELL_ERROR)] in
  assert_equal ~ctxt ~printer:[%show: reply list] expected actual

let test__functions ctxt =
  let actual = Eval_util.eval
      "%simulate 0\
       \nfloat difs = 50.;\
       \nfloat sifs = 10.;\
       \nfloat tau = 20.;\
       \nfloat timeout = sifs + tau;\
       \nfloat rts = 160.;\
       \nfloat cts = 112.;\
       \nfloat ack = 112.;\
       \nfloat t_min = 15.;\
       \nfloat rho2 = 1./(sifs + cts);\
       \nfloat rho4 = 1./(sifs + ack);\
       \nfloat rho5 = 1./(timeout);\
       \nfun ctrl S(t) = 1;\
       \nfun ctrl S_L(t) = 1;\
       \nfun ctrl S_C(t) = 1;\
       \nctrl S_E = 1;\
       \nctrl M = 2;\
       \nctrl M_L = 2;\
       \nctrl M_D = 2;\
       \nctrl M_P = 2;\
       \nctrl M_B = 2;\
       \nfun ctrl W(l) = 2;\
       \nfun ctrl RTS(l) = 2;\
       \nfun ctrl CTS(l) = 2;\
       \nfun ctrl P(l) = 2;\
       \natomic ctrl Q = 1;\
       \natomic ctrl A = 1;\
       \nfun big m_A(l) = \
       \n/x /r /q (M{r, x}.(W(l){x,a_B}.Q{q} | A{a_A}));\
       \nbig m_B =\
       \n/x /r (M{r, x}.A{a_B});\
       \nfun big m_C(k) =\
       \n/x /r /q (M{r, x}.(W(k){x,a_B}.Q{q} | A{a_C}));\
       \nbig psi =\
       \n([{0,1}, {0,1,2}, {1,2}], 3);\
       \nfun big n_0(l,k,t_A,t_B,t_C) =\
       \nshare (m_A(l) || m_B || m_C(k))\
       \nby psi\
       \nin (id{a_A,a_B,a_C} | S(t_A){a_A} | S(t_B){a_B} | S(t_C){a_C} );\
       \nbig psi_1 = ([{1}, {0,1}], 2);\
       \nfun big m_w(l) =\
       \n/x /r (M{r,x}.(id | W(l){x,d}.Q{q} | A{a}));\
       \nfun big m_l(l) =\
       \n/x /r (M_L{r,x}.(id | RTS(l){x,d}.Q{q} | A{a}));\
       \nfun react r_RTS(t,l) =\
       \nshare (id || m_w(l)) by psi_1 in (id(1, {a, d, q}) || S(t){a})\
       \n-[ 1.0/(difs + tau*t/2.0 + rts) ]->\
       \nshare (id || m_l(l)) by psi_1 in (id(1, {a, d, q}) || S_L(t){a});\
       \nbig psi_2 = ([{0,2,3}, {1,2,3}, {2}, {3}], 4);\
       \nfun big s(l) =\
       \n/x (M_L{r,x}.(id | CTS(l){x,d}.Q{q} | A{a}));\
       \nbig r = \
       \nM_L{r,x}.(id | A{d});\
       \nfun react r_CTS(t, u, l) =\
       \nshare (m_l(l) || M_D{r,x}.(id | A{d})|| id(2))\
       \nby psi_2\
       \nin (id(2, {x,a,d,q}) || (S_L(t){a} | S(u){d}) || /r)\
       \n-[ rho2 ]->\
       \nshare (s(l) || r || id(2))\
       \nby psi_2\
       \nin (id(2, {x,a,d,q}) ||(S_L(t){a} | S_L(u){d}) || /r);\
       \nfun big s1(l) =\
       \n/x (M_L{r,x}.(id | P(l){x,d}.Q{q} | A{a}));\
       \nfun react r_DATA(l) = \
       \n/r (s(l) || r)\
       \n-[ 1.0 / (sifs + l) ]->\
       \n/r (s1(l) || r);\
       \nfun big s2(l) =\
       \n/r (M{r,q}.(id | A{a}));\
       \nbig r1 = \
       \n/r (M{r,x}.(id | A{d}));\
       \nfun react r_ACK(t,u,l) = \
       \nshare (s1(l) || r || id(2))\
       \nby psi_2\
       \nin (id(2,{x,a,q,d}) || (S_L(t){a} | S_L(u){d}) || /r)\
       \n-[ rho4 ]->\
       \nshare (s2(l) || r1 || id(2))\
       \nby psi_2\
       \nin (id(2,{x,a,q,d}) || (S_C(t_min){a} | S_C(u){d}));\
       \nfun big f(t,u,l) = \
       \nshare (m_w(l) || M{r,x}.(id | A{d}) || id(2))\
       \nby psi_2\
       \nin (id(2,{x,a,q,d}) || (S_C(2.0*t+1.0){a} | S(u){d}) || /r);\
       \nfun big m_b(l) =\
       \n/x /r (M_B{r,x}.(id | RTS(l){x,d}.Q{q} | A{a}));\
       \nfun react r_BACK1(t,u,l) =\
       \nshare (m_b(l) || M_P{r,x}.(id | A{d}) || id(2))\
       \nby psi_2\
       \nin (id(2,{x,a,q,d}) || (S_L(t){a} | S(u){d}) || /r)\
       \n-[ rho5 ]->\
       \nf(t,u,l);\
       \nfun big f_E(u,l) =\
       \nshare (m_w(l) || M{r,x}.(id | A{d}) || id(2))\
       \nby psi_2\
       \nin (id(2,{x,a,q,d}) || (S_E{a} | S(u){d}) || /r);\
       \nfun react r_BACK1E(u,l) =\
       \nshare (m_b(l) || M_P{r,x}.(id | A{d}) || id(2))\
       \nby psi_2\
       \nin (id(2,{x,a,q,d}) || (S_L(1023.0){a} | S(u){d}) || /r)\
       \n-[ rho5 ]->\
       \nf_E(u,l);\
       \nfun react r_BACK2(t,u,l) =\
       \nshare (m_b(l) || M_D{r,x}.(id | A{d}) || id(2))\
       \nby psi_2\
       \nin (id(2,{x,a,q,d}) || (S_L(t){a} | S(u){d}) || /r)\
       \n-[ rho5 ]->\
       \nf(t,u,l);\
       \nfun react r_BACK2E(u,l) =\
       \nshare (m_b(l) || M_D{r,x}.(id | A{d}) || id(2))\
       \nby psi_2\
       \nin (id(2,{x,a,q,d}) || (S_L(1023.0){a} | S(u){d}) || /r)\
       \n-[ rho5 ]->\
       \nf_E(u,l);\
       \nfun react r_D(t) =\
       \nshare (id || M{r,x}.(id | A{a}))\
       \nby psi_1\
       \nin (id(1,{a,x}) || S_L(t){b} || /r)\
       \n-[ inf ]->\
       \nshare (id || M_D{r,x}.(id | A{a}))\
       \nby psi_1\
       \nin (id(1,{a,x}) || S_L(t){b} || /r);\
       \nbig psi_3 = ([{1}, {2}, {0,1,2}], 3);\
       \nfun react r_P(t,u) = \
       \nshare (id(2) || M_D{r,x}.(id | A{a}))\
       \nby psi_3\
       \nin (id(1,{a,x}) || (S_L(t){b} | S_L(u){c}) || /r)\
       \n-[ inf ]->\
       \nshare (id(2) || M_P{r,x}.(id | A{a}))\
       \nby psi_3\
       \nin (id(1,{a,x}) || (S_L(t){b} | S_L(u){c}) || /r);\
       \nfun react r_B(l) = \
       \nm_l(l) || /r M_P{r,x}.(id | A{d})\
       \n-[ inf ]->\
       \nm_b(l) || /r M_P{r,x}.(id | A{d});\
       \nfun big clr(t) =\
       \nshare (id || M{r,x}.(id | A{a}))\
       \nby psi_1\
       \nin (id(1,{a,x}) || S_C(t){b} || /r);\
       \nfun react r_UD(t) = \
       \nshare (id || M_D{r,x}.(id | A{a}))\
       \nby psi_1\
       \nin (id(1,{a,x}) || S_C(t){b} || /r)\
       \n-[ inf ]->\
       \nclr(t);\
       \nfun react r_UP(t) = \
       \nshare (id || M_P{r,x}.(id | A{a}))\
       \nby psi_1\
       \nin (id(1,{a,x}) || S_C(t){b} || /r)\
       \n-[ inf ]->\
       \nclr(t);\
       \nfun react r_UC(t) = \
       \nshare (id || M{r,x}.(id | A{a}))\
       \nby psi_1\
       \nin (id(1,{a,x}) || S_C(t){a} || /r)\
       \n-[ inf ]->\
       \nshare (id || M{r,x}.(id | A{a}))\
       \nby psi_1\
       \nin (id(1,{a,x}) || S(t){a} || /r);\
       \nbig error = S_E{x};\
       \nbig collision = M_B{x,y} || M_B{z,w};\
       \nfun big transmission(t) =\
       \nshare (id || /x M_L{r,x}.(id | P(8464.){x,d} | A{a}))\
       \nby ([{0}, {0,1}], 2)\
       \nin (S_L(t){a} || id(1,{d,r,a}));\
       \nbegin sbrs\
       \nfloat t, t' = { 15., 31., 63., 127., 255., 511., 1023. };\
       \nfloat z = { 15., 31., 63., 127., 255., 511. };\
       \nfloat l = { 8464., 4368. };\
       \ninit n_0(8464., 4368., 15., 15., 15.);\
       \nrules = [\
       \n(r_UD(t), r_UP(t)),\
       \n(r_UC(t)),\
       \n(r_D(t)),\
       \n(r_P(t,t')),\
       \n(r_B(l)),\
       \n{r_BACK1(z,t,l), r_BACK1E(t,l), r_BACK2(z,t,l), r_BACK2E(t,l)},\
       \n{r_RTS(t,l), r_CTS(t,t',l), r_DATA(l), r_ACK(t,t',l)}\
       \n];\
       \npreds = { error, collision, transmission(t) };\
       \nend" |> map_content in
  let actual_length = List.length actual in
  let actual1 = List.nth actual 1 in
  let expected = Shell (execute_reply ~count:0 SHELL_OK) in
  assert_equal ~ctxt ~printer:[%show: int] 2 actual_length ;
  assert_equal ~ctxt ~printer:[%show: reply] expected actual1

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
      "bigraphers_output" >:: test__bigraphers_output;
      "probabilistic_model_state_diagram" >::
      test__probabilistic_model_simulation;
      "incomplete_model_state_diagram" >:: test__incomplete_model_state_diagram;
      "incomplete_model_simulation" >:: test__incomplete_model_simulation;
      "stochastic_model_simulation" >:: test__stochastic_model_simulation;
      "state_diagram" >:: test__state_diagram;
      "stochastic_simulation_with_bad_argument" >::
      test__stochastic_simulation_with_bad_argument;
      "deterministic_simulation_with_bad_argument" >::
      test__deterministic_simulation_with_bad_argument;
      "functions" >:: test__functions;
    ]
  ]

let () =
  init ~init_file:"fixtures/ocamlinit.ml" () ;
  run_test_tt_main suite
