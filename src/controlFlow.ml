open Printf
type control_flow =
  | Ast of Ast.ast
  | If of control_flow * (control_flow list)
  | For of control_flow * control_flow * control_flow * (control_flow list)
  | None

let rec to_string ast =
  let _to_string ast =
    match ast with
    | Ast _ast -> sprintf "%s" (Ast.to_string _ast)
    | _ -> "" in
  match ast with
  | Ast _ast -> Ast.sprint_ast _ast
  | If (cond, procs) ->
    let header = sprintf "if(%s) {" (_to_string cond) in
    let body = List.map to_string procs in
    let footer = "}" in
    String.concat "" (List.flatten [[header]; body; [footer]])
  | For (init, cond, update, procs) -> ""
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

(* let restore_for asts = *)
(*   let rec _restore asts used = *)
(*     match asts with *)
(*     | [] -> [None] *)
(*     | ast::_asts -> *)
(*       match ast with *)
(*       | Ast (Ast.Jmp lbl) -> *)
(*         let proc, cond, update, remain =  *)
(*           match take_while (fun x -> not (x = Ast lbl)) _asts with *)
(*           | [], [] -> [], Ast.Emp, Ast.Emp, [] *)
(*           | ls, [] -> [], Ast.Emp, Ast.Emp, [] *)
(*           | [], rs -> [], Ast.Emp, Ast.Emp, [] *)
(*           | l::ls, r::rs -> *)
(*             match l, r with *)
(*             | (Ast (Ast.Label lbl), Ast (Ast.JmpCond (cond, _))) -> *)
(*               ls, cond, List.hd rs, List.tl rs *)
(*             | _ -> [], Ast.Emp, Ast.Emp, [] *)
(*             | _ -> [], Ast.Emp, Ast.Emp, [] *)
(*         in *)
(*         let init = List.hd used in *)
(*         (For (init, Ast cond, Ast update, proc)) :: (_restore remain) *)
(*       | _ -> [None] in *)
