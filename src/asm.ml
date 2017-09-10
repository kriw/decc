open Printf
type reg = Eax | Ebx | Ecx | Edx | Edi | Esi | Else

type operand =
  | Imm of int32
  | Local of string
  | Arg of string
  | Reg of reg
  | Unknown

type asm =
  | Nop
  | Mov of operand * operand
  | Lea of operand * operand
  | Add of operand * operand
  | Sub of operand * operand
  | Mul of operand * operand
  | Div of operand
  | Cmp of operand * operand
  | Jmp of operand
  | Call of operand
  | Ret
  | Unknown

let is_reg s =
  let reg_pattern1 = Str.regexp "e[abcd]x" in
  let reg_pattern2 = Str.regexp "e[ds]i" in
  let reg_pattern3 = Str.regexp "e[sbi]p" in
  (Str.string_match  reg_pattern1 s 0) ||
  (Str.string_match  reg_pattern2 s 0) ||
  (Str.string_match  reg_pattern3 s 0)

let to_reg str =
  match str with
  | "eax" -> Eax
  | "ebx" -> Ebx
  | "ecx" -> Ecx
  | "edx" -> Edx
  | "edi" -> Edi
  | "esi" -> Esi
  | _ -> Else

let is_imm s =
  let imm_pattern = Str.regexp "0x[0-9a-fA-F]+" in
  Str.string_match imm_pattern s 0

let loc_ptn =
  Str.regexp "DWORD PTR \\[ebp-\\(0x[0-9a-fA-F]+\\)\\]"
let is_local s = Str.string_match loc_ptn s 0
let to_local s = Str.global_replace loc_ptn "\\1" s

let arg_ptn =
  Str.regexp "DWORD PTR \\[ebp\\+\\(0x[0-9a-fA-F]+\\)\\]"
let is_arg s = Str.string_match arg_ptn s 0
let to_arg s = Str.global_replace arg_ptn "\\1" s

let to_operand op =
  match op with
  (* TODO *)
  (* | op when is_call op -> Label  *)
  | op when is_imm op -> Imm (Int32.of_string op)
  | op when is_reg op -> Reg (to_reg op)
  | op when is_local op -> Local (to_local op)
  | op when is_arg op -> Arg (to_arg op)
  | _ -> print_endline "fail"; print_endline op; Unknown


(* TODO *)
(* let call_regex = Str.regexp "\\([0-9a-fA-F]+\\) <\\(.*\\)>" *)
(* let is_call op = Str.string_match call_regex op 0 *)
(* let parse_call_op op = *)
(*   let addr = Str.global_replace call_regex "0x\\1" op in *)
(*   let label = Str.global_replace call_regex "\\2" op in *)
(*   label, addr *)

let to_asm mnem ops =
  let first l = to_operand (List.nth l 0) in
  let second l = to_operand (List.nth l 1) in
  match mnem with
  | "nop" -> Nop
  | "mov" -> Mov ((first ops), (second ops))
  | "add" -> Add ((first ops), (second ops))
  | "sub" -> Sub ((first ops), (second ops)) 
  | "mul" | "imul" -> Mul ((first ops), (second ops))
  | "div" | "idiv" -> Div (first ops)
  | "jmp" -> Jmp (first ops)
  | "call" -> Call (first ops)
  | "ret" -> Ret
  | _ -> Unknown

let reg_to_string reg =
  match reg with
  | Eax -> "eax"
  | Ebx -> "ebx"
  | Ecx -> "ecx"
  | Edx -> "edx"
  | Edi -> "edi"
  | Esi -> "esi"
  | Else -> "else"

let op_to_string op =
  match op with
  | Imm n -> sprintf "0x%x" (Int32.to_int n)
  | Local lbl -> sprintf "%s_%s" "local" lbl
  | Arg lbl -> sprintf "%s_%s" "arg" lbl
  | Reg reg -> reg_to_string reg
  | Unknown -> "unknown"


let to_string asm =
  let of_op = op_to_string in
  let concat op1 op2 = String.concat ", " [op1; op2] in
  let mnem_str, op_str =
    match asm with
    | Nop -> "nop", ""
    | Mov (op1, op2) -> "mov", concat (of_op op1) (of_op op2)
    | Lea (op1, op2) -> "lea", concat (of_op op1) (of_op op2)
    | Add (op1, op2) -> "add", concat (of_op op1) (of_op op2)
    | Sub (op1, op2) -> "sub", concat (of_op op1) (of_op op2)
    | Mul (op1, op2) -> "mul", concat (of_op op1) (of_op op2)
    | Div op -> "div", of_op op
    | Cmp (op1, op2) -> "cmp", concat (of_op op1) (of_op op2)
    | Jmp op -> "jmp", of_op op
    | Call op -> "call", of_op op
    | Ret -> "ret", ""
    | Unknown -> "unknown", "" in
  Printf.sprintf "%s %s" mnem_str op_str
