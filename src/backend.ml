open Printf
let emit_func func_name asts =
  let header = sprintf "%s() {\n" func_name in
  let body = String.concat "\n" (List.map Ast.to_string asts) in
  let footer = "\n}" in
  String.concat "" [header; body; footer]
