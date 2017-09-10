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

let get_state_reg line =
  let operand = match line with
    | Asm.Mov (op, _) -> op
    | Asm.Lea (op, _) -> op
    | Asm.Add (op, _) -> op
    | Asm.Sub (op, _) -> op
    | Asm.Mul (op, _) -> op
    | Asm.Div op -> op
    | Asm.Call op -> op
    | _ -> Asm.Unknown in
  let reg = match operand with
    | Asm.Reg reg -> reg
    | _ -> Asm.Else in
  state_reg reg

let emit_ast line =
  let reg = get_state_reg line in
  let ast = match line with
    | Asm.Mov (op1, op2) -> 
      ref (Assign (!(state_ast op1), !(state_ast op2)))
    | Asm.Lea (op1, op2) -> 
      ref (Assign (!(state_ast op1), Ref !(state_ast op2)))
    | Asm.Add (op1, op2) -> 
      ref (Add (!(state_ast op1), !(state_ast op2)))
    | Asm.Sub (op1, op2) -> 
      ref (Sub (!(state_ast op1), !(state_ast op2)))
    | Asm.Mul (op1, op2) ->
      ref (Mul (!(state_ast op1), !(state_ast op2)))
    | Asm.Div op ->
      ref (Div (!State.eax, !(state_ast op)))
    | Asm.Cmp (op1, op2) ->
      ref (Cond (!(state_ast op1), !(state_ast op2)))
    | Asm.Jmp op ->
      ref (Jmp !(state_ast op))
    | Asm.Call op ->
      ref (Call !(state_ast op))
    | Asm.Ret ->
      ref (Ret !State.eax)
    | _ -> ref Emp in
  let _ = match !reg with
    | Emp -> ()
    | _ -> reg := !ast in
  ast

(* TODO *)
let rec to_ast asms =
  match asms with
  | [] -> ref Emp
  | x::xs -> 
    let ast = emit_ast x in
    ast
