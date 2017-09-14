open Printf

type ast =
  | Add of ast * ast
  | Sub of ast * ast
  | Mul of ast * ast
  | Div of ast * ast
  | Assign of ast * ast
  | JmpCond of ast * ast
  | Jmp of ast
  | Cond of ast * ast
  | Call of ast * ast list
  | Ret of ast
  | Ref of ast
  | Equal of ast
  | NotEqual of ast
  | Above of ast
  | AboveEq of ast
  | Below of ast
  | BelowEq of ast
  | And of ast * ast
  | Or of ast * ast
  | Label of string
  | Value of Asm.operand
  | Emp

module State = struct
  let eax = ref Emp
  let ebx = ref Emp
  let ecx = ref Emp
  let edx = ref Emp
  let edi = ref Emp
  let esi = ref Emp
  let esp = ref [] (*Stack*)
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
  | Asm.FuncLabel _ -> ref (Value op)
  | Asm.Label _ -> ref (Label (Asm.op_to_string op))
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

let is_jmp_mnem line =
  match line with
  | Asm.Jmp _ -> true
  | Asm.Jne _ -> true
  | Asm.Jle _ -> true
  | Asm.Jge _ -> true
  | Asm.Ret  -> true
  | _ -> false

let is_local op =
  match op with
  | Asm.Local str -> true
  | _ -> false

let is_local_from_op1 line =
  let operand = first_op line in
  is_local operand

let is_reg_from_op1 line =
  let operand = first_op line in
  match operand with
  | Asm.Reg reg -> true
  | _ -> false

let emit_ast line =
  match line with
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
    let _ = if is_local op1 then
        a := (Assign (!(state_ast op1), !a))
      else 
        (state_ast op1) :=  !a in
    a
  | Asm.Sub (op1, op2) -> 
    let a = ref (Sub (!(state_ast op1), !(state_ast op2))) in
    let _ = if is_local op1 then
        a := (Assign (!(state_ast op1), !a))
      else
        (state_ast op1) := !a in
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
  | Asm.Jle op -> ref (JmpCond (BelowEq !State.eflags, !(state_ast op)))
  | Asm.Jge op -> ref (JmpCond (AboveEq !State.eflags, !(state_ast op)))
  | Asm.Je op -> ref (JmpCond (NotEqual !State.eflags, !(state_ast op)))
  | Asm.Jne op -> ref (JmpCond (Equal !State.eflags, !(state_ast op)))
  | Asm.Jmp op -> ref (Jmp !(state_ast op))
  | Asm.Push op -> State.esp := !(state_ast op)::(!State.esp); ref Emp
  | Asm.Call op ->
    let a = ref (Call (!(state_ast op), !State.esp)) in
    let _ = State.esp := [] in
    let _ = State.eax := !a in
    a
  | Asm.Label lbl -> ref (Label (Asm.op_to_string lbl))
  | Asm.Ret -> ref (Ret !State.eax)
  | _ -> ref Emp

let rec to_string ast =
  match ast with
  | Add (ast1, ast2) -> sprintf "(%s + %s)" (to_string ast1) (to_string ast2)
  | Sub (ast1, ast2) -> sprintf "(%s - %s)" (to_string ast1) (to_string ast2)
  | Mul (ast1, ast2) -> sprintf "(%s * %s)" (to_string ast1) (to_string ast2)
  | Div (ast1, ast2) -> sprintf "(%s / %s)" (to_string ast1) (to_string ast2)
  | Assign (ast1, ast2) -> sprintf "%s = %s" (to_string ast1) (to_string ast2)
  | JmpCond (ast1, ast2) -> sprintf "if(%s) goto %s" (to_string ast1) (to_string ast2)
  | Jmp ast -> sprintf "goto %s" (to_string ast)
  | Equal Cond (ast1, ast2) -> sprintf "(%s == %s)" (to_string ast1) (to_string ast2)
  | NotEqual Cond (ast1, ast2) -> sprintf "(%s == %s)" (to_string ast1) (to_string ast2)
  | Below Cond (ast1, ast2) -> sprintf "(%s < %s)" (to_string ast1) (to_string ast2)
  | BelowEq Cond (ast1, ast2) -> sprintf "(%s <= %s)" (to_string ast1) (to_string ast2)
  | Above Cond (ast1, ast2) -> sprintf "(%s > %s)" (to_string ast1) (to_string ast2)
  | AboveEq Cond (ast1, ast2) -> sprintf "(%s >= %s)" (to_string ast1) (to_string ast2)
  | And (ast1, ast2) -> sprintf "(%s & %s)" (to_string ast1) (to_string ast2)
  | Or (ast1, ast2) -> sprintf "(%s | %s)" (to_string ast1) (to_string ast2)
  | Call (ast, args) -> sprintf "%s(%s)" (to_string ast) (String.concat ", " (List.map to_string args))
  | Ret ast -> sprintf "return %s" (to_string ast)
  | Ref ast -> sprintf "&(%s)"  (to_string ast)
  | Value op -> Asm.op_to_string op
  | Label lbl -> sprintf "%s:" lbl
  | Emp -> ""
  | _ -> "unknown"

let sprint_ast ast = match ast with
  | Label _ -> sprintf "%s\n" (to_string ast)
  | _ -> sprintf "%s;\n" (to_string ast)
let print_ast ast = match ast with
  | Label _ -> print_endline (to_string ast)
  | _ -> printf "%s;\n" (to_string ast)

(* FIXME *)
let skip line =
  match line with
  | Asm.Div _ -> true
  | _ -> false

let is_label ast = 
  match ast with
  | Asm.Label _ -> true
  | _ -> false

let rec is_used ast asts =
  match asts with
  | [] -> false
  | _ast::_asts -> 
    if ast = _ast then true
    else is_used ast _asts

let to_ast asms = 
  let rec _to_ast asms ast =
    match asms with
    | [] -> List.rev ast
    | x::xs -> 
      let _ast = emit_ast x in
      match x with
      (* FIXME better solution *)
      | x when skip x -> _to_ast xs ast
      | Asm.Call _ when not (is_used x xs) -> _to_ast xs ((!_ast)::ast)
      | x when (is_local_from_op1 x) || (is_jmp_mnem x) || (is_label x) -> 
        _to_ast xs ((!_ast)::ast)
      | _ -> _to_ast xs ast in
  _to_ast asms []
