open Printf
open ControlFlow

let emit_args_label n =
  let to_label n = sprintf "arg_%d" n in
  let rec _emit n ret =
    match n with
    | 0 -> ret
    | n -> _emit (n-1) ((to_label n)::ret) in
  _emit n []

let emit_func func_name asts arg_num =
  let args = emit_args_label arg_num in
  let header = sprintf "%s(%s) {\n" func_name (String.concat ", " args) in
  let body = String.concat "" (List.map ControlFlow.to_string asts) in
  let footer = "\n}" in
  String.concat "" [header; body; footer]

