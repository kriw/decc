open Printf

type ast =
  | Add of ast * ast
  | Sub of ast * ast
  | Mul of ast * ast
  | Div of ast * ast
  | Assign of ast * ast
  | Jmp of ast
  | Cond of ast * ast
  | Call of ast
  | Ret of ast
  | Ref of ast
  | Value of Asm.operand
  | Emp

module State = struct
  let eax = ref Emp
  let ebx = ref Emp
  let ecx = ref Emp
  let edx = ref Emp
  let edi = ref Emp
  let esi = ref Emp
  (* let ebp = ref Emp *)
  (* let esp = ref Emp *)
  (* let eip = ref Emp *)
  (* let eflags = ref Emp *)
end

let ast = []

let state_local = 1
let state_arg = 1
let state_reg reg =
  match reg with
  | Asm.Eax -> State.eax
  | Asm.Ebx -> State.ebx
  | Asm.Ecx -> State.ecx
  | Asm.Edx -> State.edx
  | Asm.Edi -> State.edx
  | Asm.Esi -> State.esi
  | _ -> ref Emp

let state_ast op =
  match op with
  | Asm.Imm _ -> ref (Value op)
  | Asm.Local _ -> ref (Value op)
  | Asm.Arg _ -> ref (Value op)
  | Asm.Reg reg -> state_reg reg
  | _ -> ref Emp

let eat_mov op1 op2 =
  match op1 with
  | Asm.Reg reg -> ()
  | _ -> ()

let first_op line = 
  match line with
  | Asm.Mov (op, _) -> op
  | Asm.Lea (op, _) -> op
  | Asm.Add (op, _) -> op
  | Asm.Sub (op, _) -> op
  | Asm.Mul (op, _) -> op
  | Asm.Div op -> op
  | Asm.Call op -> op
  | _ -> Asm.Unknown

let is_func_mnem line =
  match line with
  | Asm.Call _ -> true
  | Asm.Ret  -> true
  | _ -> false

let is_local_from_op1 line =
  let operand = first_op line in
  match operand with
  | Asm.Local str -> true
  | _ -> false

let is_reg_from_op1 line =
  let operand = first_op line in
  match operand with
  | Asm.Reg reg -> true
  | _ -> false

let emit_ast line =
  let op1_is_reg = is_reg_from_op1 line in
  let ast = match line with
    | Asm.Mov (op1, op2) -> 
      let a = ref (Assign (!(state_ast op1), !(state_ast op2))) in
      let _ = if op1_is_reg then
          (state_ast op1) := !(state_ast op2)
        else () in
      a
    | Asm.Lea (op1, op2) -> 
      let a = ref (Assign (!(state_ast op1), Ref !(state_ast op2))) in
      let _ = if op1_is_reg then
          (state_ast op1) := Ref !(state_ast op2)
        else () in
      a
    | Asm.Add (op1, op2) -> 
      let a = ref (Add (!(state_ast op1), !(state_ast op2))) in
      let _ = if op1_is_reg then
          (state_ast op1) := !a
        else () in
      a
    | Asm.Sub (op1, op2) -> 
      let a = ref (Sub (!(state_ast op1), !(state_ast op2))) in
      let _ = if op1_is_reg then
          (state_ast op1) := !a
        else () in
      a
    | Asm.Mul (op1, op2) ->
      let a = ref (Mul (!(state_ast op1), !(state_ast op2))) in
      let _ = if op1_is_reg then
          (state_ast op1) := !a
        else ()in
      a
    | Asm.Div op ->
      let a = ref (Div (!State.eax, !(state_ast op))) in
      let _ = if op1_is_reg then
          (state_ast op) := !a
        else () in
      a
    | Asm.Cmp (op1, op2) ->
      ref (Cond (!(state_ast op1), !(state_ast op2)))
    | Asm.Jmp op ->
      ref (Jmp !(state_ast op))
    | Asm.Call op ->
      ref (Call !(state_ast op))
    | Asm.Ret ->
      ref (Ret !State.eax)
    | _ -> ref Emp in
  ast

let rec to_string ast =
  match ast with
  | Add (ast1, ast2) -> sprintf "(%s + %s)" (to_string ast1) (to_string ast2)
  | Sub (ast1, ast2) -> sprintf "(%s - %s)" (to_string ast1) (to_string ast2)
  | Mul (ast1, ast2) -> sprintf "(%s * %s)" (to_string ast1) (to_string ast2)
  | Div (ast1, ast2) -> sprintf "(%s / %s)" (to_string ast1) (to_string ast2)
  | Assign (ast1, ast2) -> sprintf "%s = %s" (to_string ast1) (to_string ast2)
  | Jmp ast -> "" (* TODO *)
  | Cond (ast1, ast2) -> "" (* TODO *)
  | Call ast -> "" (* TODO *)
  | Ret ast -> sprintf "return %s" (to_string ast)
  | Ref ast -> sprintf "&(%s)"  (to_string ast)
  | Value op -> Asm.op_to_string op
  | Emp -> "emp"

let to_ast asms = 
  let rec _to_ast asms ast =
    match asms with
    | [] -> List.rev ast
    | x::xs -> 
      let _ast = emit_ast x in
      match x with
      | x when (is_local_from_op1 x) || (is_func_mnem x) -> 
        _to_ast xs ((!_ast)::ast)
      | _ -> _to_ast xs ast in
  _to_ast asms []
