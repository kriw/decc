(* TODO *)type cond = unit
type reg = Eax | Ebx | Ecx | Edx | Edi | Esi | Else
type addr = Addr of int32
type label = Label of string * int32
type mem = 
  | Imm of addr
  | Local of reg * int32
  | Arg of reg * int32
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
  | Mov of operand * operand
  | Lea of operand * operand
  | Jmp of label * cond
  | Call of label
  | Ret

let to_reg str =
  match str with
  | "eax" -> Eax
  | "ebx" -> Ebx
  | "ecx" -> Ecx
  | "edx" -> Edx
  | "edi" -> Edi
  | "esi" -> Esi
  | _ -> Else

let label_n = ref 0
let gen_label =
  let n = !label_n in
  let _ = label_n := !label_n + 1 in
  Int32.of_int n
let to_label addr = Label (addr, gen_label)
let to_mem = 1
let to_expr = 1
let to_operand op = 1
let to_insn mnem ops =
  match mnem with
  | "call" -> "call"
  | "mov" -> "mov"
  | "lea" -> "lea"
  | "jmp" -> "jmp"
  | "jle" -> "jmp less than equal"
  | "ret" -> "ret"
  | _ -> "unknown"

