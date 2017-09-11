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
  | Equal of ast
  | Above of ast
  | Below of ast
  | And of ast * ast
  | Or of ast * ast
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
  let eflags = ref Emp
end

let state_reg reg =
  match reg with
  (* FIXME separate reg and reg[:8] *)
  | Asm.Eax | Asm.Al -> State.eax
  | Asm.Ebx | Asm.Bl -> State.ebx
  | Asm.Ecx | Asm.Cl -> State.ecx
  | Asm.Edx | Asm.Dl -> State.edx
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
  let ast = match line with
    | Asm.Mov (op1, op2) -> 
      let a = ref (Assign (!(state_ast op1), !(state_ast op2))) in
      let _ = (state_ast op1) := !(state_ast op2) in
      a
    | Asm.Lea (op1, op2) -> 
      let a = ref (Assign (!(state_ast op1), Ref !(state_ast op2))) in
      let _ = (state_ast op1) := Ref !(state_ast op2) in
      a
    | Asm.Add (op1, op2) -> 
      let a = ref (Add (!(state_ast op1), !(state_ast op2))) in
      let _ = (state_ast op1) := !a in
      a
    | Asm.Sub (op1, op2) -> 
      let a = ref (Sub (!(state_ast op1), !(state_ast op2))) in
      let _ = (state_ast op1) := !a in
      a
    | Asm.Mul (op1, op2) ->
      let a = ref (Mul (!(state_ast op1), !(state_ast op2))) in
      let _ = (state_ast op1) := !a in
      a
    | Asm.Div op ->
      let a = ref (Div (!State.eax, !(state_ast op))) in
      let _ = (State.eax) := !a in
      a
    | Asm.Cmp (op1, op2) ->
      let a = ref (Cond (!(state_ast op1), !(state_ast op2))) in
      let _ = State.eflags := !a in
      a
    (* FIXME assign not to reg, but to reg[:8]  *)
    | Asm.Sete op ->
      let a = ref (Equal !State.eflags) in
      let _ = (state_ast op) := !a in
      a
    | Asm.Setg op ->
      let a = ref (Above !State.eflags) in
      let _ = (state_ast op) := !a in
      a
    | Asm.Setl op ->
      let a = ref (Below !State.eflags) in
      let _ = (state_ast op) := !a in
      a
    | Asm.And (op1, op2) ->
      let a = ref (And (!(state_ast op1), !(state_ast op2))) in
      let _ = (state_ast op1) := !a in
      a
    | Asm.Or (op1, op2) ->
      let a = ref (Or (!(state_ast op1), !(state_ast op2))) in
      let _ = (state_ast op1) := !a in
      a
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
  | Equal Cond (ast1, ast2) -> sprintf "(%s == %s)" (to_string ast1) (to_string ast2)
  | Below Cond (ast1, ast2) -> sprintf "(%s < %s)" (to_string ast1) (to_string ast2)
  | Above Cond (ast1, ast2) -> sprintf "(%s > %s)" (to_string ast1) (to_string ast2)
  | And (ast1, ast2) -> sprintf "(%s & %s)" (to_string ast1) (to_string ast2)
  | Or (ast1, ast2) -> sprintf "(%s | %s)" (to_string ast1) (to_string ast2)
  | Call ast -> "" (* TODO *)
  | Ret ast -> sprintf "return %s" (to_string ast)
  | Ref ast -> sprintf "&(%s)"  (to_string ast)
  | Value op -> Asm.op_to_string op
  | Emp -> "emp"
  | _ -> "unknown"

(* FIXME *)
let skip line =
  match line with
  | Asm.Div _ -> true
  | _ -> false

let to_ast asms = 
  let rec _to_ast asms ast =
    match asms with
    | [] -> List.rev ast
    | x::xs -> 
      let _ast = emit_ast x in
      match x with
      (* FIXME better solution *)
      | x when skip x -> _to_ast xs ast
      | x when (is_local_from_op1 x) || (is_func_mnem x) -> 
        _to_ast xs ((!_ast)::ast)
      | _ -> _to_ast xs ast in
  _to_ast asms []
