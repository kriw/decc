open Printf
type control_flow =
  | Ast of Ast.ast
  | If of control_flow * (control_flow list)
  | For of control_flow * control_flow * control_flow * (control_flow list)
  | None

let rec to_not ast =
  match ast with
  | Ast _ast -> Ast (Ast.to_not _ast)
  | _ -> ast

let rec to_string ast =
  let _to_string ast =
    match ast with
    | Ast _ast -> sprintf "%s" (Ast.to_string _ast)
    | _ -> "" in
  match ast with
  | Ast _ast -> Ast.sprint_ast _ast
  | If (cond, procs) ->
    let header = sprintf "if(%s) {" (_to_string (to_not cond)) in
    let body = List.map to_string procs in
    let footer = "}" in
    String.concat "" (List.flatten [[header]; body; [footer]])
  | For (init, cond, update, procs) ->
    let header = sprintf "for(%s; %s; %s) {"
        (_to_string init) (_to_string cond) (_to_string update) in
    let body = List.map to_string procs in
    let footer = "}" in
    String.concat "" (List.flatten [[header]; body; [footer]])
  | _ -> ""

let rec from_ast asts =
  let rec _from_ast asts ret =
    match asts with
    | [] -> List.rev ret
    | ast::_asts -> _from_ast _asts ((Ast ast)::ret) in
  _from_ast asts []

let take_while f ls =
  let rec _take_while ls l r =
    match ls with
    | [] -> print_endline "UNCHI"; l, []
    | x::xs when f x ->  _take_while xs (x::l) r
    | x::xs when not (f x) -> (List.rev (x::l)), xs 
    | _ -> [], [] in
  _take_while ls [] []

let rec restore_if asts =
  let exists lbl asts = List.exists (fun x -> x = Ast lbl) asts in
  match asts with
  | [] -> []
  | ast::_asts -> 
    match ast with
    | Ast (Ast.JmpCond (cond, lbl)) when exists lbl _asts ->
      let left, right = take_while (fun x -> not (x = Ast lbl)) _asts in
      let _if = If (Ast cond, (restore_if left)) in
      _if::(restore_if right)
    | _ -> ast :: (restore_if _asts)

let restore_for asts =
  let rec _restore asts ret =
    match asts with
    | [] -> List.rev ret
    | ast::[] -> List.rev (ast::ret)
    | init::jmp::_asts ->
      match jmp with
      | Ast (Ast.Jmp lbl0) ->
        let index_lbl0 = Util.find_index (fun x -> x = Ast lbl0) _asts in
        let cond, lbl1 = 
          match List.nth _asts (index_lbl0+1) with
          | Ast (Ast.JmpCond (cond, lbl)) -> cond, lbl
          | _ -> Ast.Emp, Ast.Emp in
        let index_lbl1 = Util.find_index (fun x -> x = Ast lbl1) _asts in
        if index_lbl1 == 0 then
          let procs = Util.take _asts (index_lbl0-1) in
          let update = List.nth  _asts (index_lbl0-1) in
          let remains = Util.drop _asts (index_lbl0+2) in
          let for_st = For (init, Ast cond, update, procs) in
          List.append (List.rev ret) (for_st::remains)
        else 
          let _ = printf "index_lbl1 %d" index_lbl1 in
          _restore (jmp::_asts) (init::ret)
      | _ -> _restore (jmp::_asts) (init::ret)
  in
  _restore asts []

let delete_labels asts =
  let rec _del asts ret =
    match asts with
    | [] -> List.rev ret
    | (Ast (Ast.Label _))::_asts -> _del _asts ret
    | If (cond, proc)::_asts -> _del _asts ((If (cond, _del proc []))::ret)
    | For (init, cond, update, proc)::_asts ->
      _del _asts ((For (init, cond, update, _del proc []))::ret)
    | ast::_asts -> _del _asts (ast::ret) in
  _del asts []

let restore_control_flow asts =
  let with_if = restore_if asts in
  let with_for = restore_for with_if in
  delete_labels with_for
