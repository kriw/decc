(* TODO *)type cond = unit
type reg = Eax | Ebx | Ecx | Edx | Edi | Esi 
type addr = int32
type label = addr
type mem = 
  | Imm of addr
  | Direct of reg * int32   (*  reg + n  *)
  | Indirect of reg * int32 (* [reg + n] *)
type expr =
  | Add of expr * expr
  | Sub of expr * expr
  | Mul of expr * expr
  | Div of expr
  | Reg of reg
  | Mem of mem
  | Val of int32
type operand = 
  | Imm of int32
  | Reg of reg
  | Mem of mem
type insn = 
  | Calc of expr
  | Mov of string * operand * operand
  | Lea of string * operand * operand
  | Jmp of label * cond
  | Call of label
  | Ret
