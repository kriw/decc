(* TODO *)type cond = unit
let str_regs = ["eax"; "ebx"; "ecx"; "edx"; "edi"; "esi"]
type reg = Eax | Ebx | Ecx | Edx | Edi | Esi | Else
type addr = Addr of int32
type label = Label of string * int32
type mem = 
  | Local of int32
  | Arg of int32
  | UnknownMem

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
  | Unknown

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

(* FIXME *)
let is_local str =
  let pattern = Str.regexp "DWORD PTR \\[ebp-0x[0-9a-fA-F]\\]" in
  Str.string_match pattern str 0

(* FIXME *)
let is_arg str =
  let pattern = Str.regexp "DWORD PTR \\[ebp+0x[0-9a-fA-F]\\]" in
  Str.string_match pattern str 0

(* FIXME *)
let to_local str = Int32.of_int 1
let to_arg str = Int32.of_int 1

let to_mem str =
  match str with
  | _str when is_local _str -> Local (to_local _str)
  | _str when is_arg _str -> Arg (to_arg str)
  | _ -> UnknownMem

(* TODO *)
let to_expr = 1

let is_imm op = 
  let pattern = Str.regexp "0x[a-zA-Z0-9]+" in
  Str.string_match pattern op 0

let is_reg op = List.mem op str_regs
let to_operand op =
  match op with
  | _op when is_imm _op -> Imm (Int32.of_string _op)
  | _op when is_reg _op -> Reg (to_reg _op)
  | _ -> Mem (to_mem op)

let to_insn mnem ops =
  match mnem with
  | "call" -> Call (to_label (List.nth ops 0))
  | "mov" -> Mov (to_operand (List.nth ops 0), to_operand (List.nth ops 1))
  | "lea" -> Lea (to_operand (List.nth ops 0), to_operand (List.nth ops 1))
  | "jmp" -> Jmp (to_label (List.nth ops 0), ())
  | "jle" -> Jmp (to_label (List.nth ops 0), ())
  | "ret" -> Ret
  | _ -> Unknown

