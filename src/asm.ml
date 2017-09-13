open Printf
module LabelMap = Map.Make(String);;


type reg = Eax | Ebx | Ecx | Edx | Edi | Esi 
         | Al | Bl | Cl | Dl | Else

type operand =
  | Imm of int32
  | Local of string
  | Arg of string
  | Reg of reg
  | FuncLabel of string
  | Label of string
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
  | Sete of operand
  | Setl of operand
  | Setg of operand
  | And of operand * operand
  | Or of operand * operand
  | Je of operand
  | Jne of operand
  | Jge of operand
  | Jle of operand
  | Jmp of operand
  | Push of operand
  | Call of operand
  | Ret
  | Label of operand
  | Unknown

let op_list asm =
  match asm with
  | Nop -> []
  | Mov (op1, op2) -> [op1; op2]
  | Lea (op1, op2) -> [op1; op2]
  | Add (op1, op2) -> [op1; op2]
  | Sub (op1, op2) -> [op1; op2]
  | Mul (op1, op2) -> [op1; op2]
  | Div op -> [op]
  | Cmp (op1, op2) -> [op1; op2]
  | Sete op -> [op]
  | Setg op -> [op]
  | Setl op -> [op]
  | And (op1, op2) -> [op1; op2]
  | Or (op1, op2) -> [op1; op2]
  | Je op -> [op]
  | Jne op -> [op]
  | Jge op -> [op]
  | Jle op -> [op]
  | Jmp op -> [op]
  | Push op -> [op]
  | Call op -> [op]
  | Ret -> []
  | Label lbl -> [lbl]
  | Unknown -> []

let is_reg s =
  let reg_pattern1 = Str.regexp "e[abcd]x" in
  let reg_pattern2 = Str.regexp "e[ds]i" in
  let reg_pattern3 = Str.regexp "e[sbi]p" in
  let reg_pattern4 = Str.regexp "[abcd]l" in
  (Str.string_match  reg_pattern1 s 0) ||
  (Str.string_match  reg_pattern2 s 0) ||
  (Str.string_match  reg_pattern3 s 0) ||
  (Str.string_match  reg_pattern4 s 0)

let to_reg str =
  match str with
  | "eax" -> Eax
  | "ebx" -> Ebx
  | "ecx" -> Ecx
  | "edx" -> Edx
  | "edi" -> Edi
  | "esi" -> Esi
  | "al" -> Al
  | "bl" -> Bl
  | "cl" -> Cl
  | "dl" -> Dl
  | _ -> Else

let is_imm s =
  let imm_pattern = Str.regexp "0x[0-9a-fA-F]+" in
  Str.string_match imm_pattern s 0

let lbl_ptn = Str.regexp "\\([0-9a-fA-F]+\\) <\\(.*\\)>"
let is_label s = Str.string_match lbl_ptn s 0
let to_label s = Str.global_replace lbl_ptn "\\1,\\2" s

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
  | op when is_label op ->
    let label = Str.global_replace (Str.regexp "\\(.*\\) <.*>") "\\1" op in
    Label label
  | _ -> print_endline "fail"; print_endline op; Unknown

let match_set_eflag mnem =
  let pattern = Str.regexp "set[egl]" in
  Str.string_match pattern mnem 0

let to_asm mnem ops =
  let first l = to_operand (List.nth l 0) in
  let second l = to_operand (List.nth l 1) in
  match mnem with
  | "nop" -> Nop
  | "mov" | "movzx" -> Mov ((first ops), (second ops))
  | "add" -> Add ((first ops), (second ops))
  | "sub" -> Sub ((first ops), (second ops)) 
  | "mul" | "imul" -> Mul ((first ops), (second ops))
  | "div" | "idiv" -> Div (first ops)
  | "sete" -> Sete (first ops)
  | "setg" -> Setg (first ops)
  | "setl" -> Setl (first ops)
  | "and" -> And ((first ops), (second ops))
  | "or" -> Or ((first ops), (second ops))
  | "cmp" -> Cmp ((first ops), (second ops))
  | "je" -> Je  (first ops)
  | "jne" -> Jne (first ops)
  | "jge" -> Jge  (first ops)
  | "jle" -> Jle  (first ops)
  | "jmp" -> Jmp (first ops)
  | "push" -> Push (first ops)
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
  | Al -> "al"
  | Bl -> "bl"
  | Cl -> "cl"
  | Dl -> "dl"
  | Else -> "else"

let op_to_string op =
  match op with
  | Imm n -> sprintf "0x%x" (Int32.to_int n)
  | Local lbl -> sprintf "%s_%s" "local" lbl
  | Arg lbl -> sprintf "%s_%s" "arg" lbl
  | Reg reg -> reg_to_string reg
  | FuncLabel lbl -> lbl
  | Label lbl -> lbl
  | Unknown -> "unknown"

module ArgSet = Set.Make(String);;
let count_arg asms =
  let arg_set = ref ArgSet.empty in
  let rec _count asms =
    match asms with
    | [] -> ()
    | asm::_asms -> 
      List.iter (fun x ->
          match x with
          | Arg lbl -> arg_set := ArgSet.add lbl !arg_set
          | _ -> ()
        ) (op_list asm);
      _count _asms in
  let () = _count asms in
  ArgSet.cardinal !arg_set

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
    (* FIXME *)
    | Sete op -> "sete", of_op op
    | Setg op -> "setg", of_op op
    | Setl op -> "setl", of_op op
    | And (op1, op2) -> "and", concat (of_op op1) (of_op op2)
    | Or (op1, op2) -> "or", concat (of_op op1) (of_op op2)
    | Je op -> "je", of_op op
    | Jne op -> "jne", of_op op
    | Jge op -> "jge", of_op op
    | Jle op -> "jle", of_op op
    | Jmp op -> "jmp", of_op op
    | Push op -> "push", of_op op
    | Call op -> "call", of_op op
    | Ret -> "ret", ""
    | Label lbl -> sprintf "%s:" (of_op lbl), ""
    | Unknown -> "unknown", "" in
  Printf.sprintf "%s %s" mnem_str op_str

let lbl_index = ref 0
let get_lbl () =
  let ret = !lbl_index in
  let _ = lbl_index := !lbl_index + 1 in
  sprintf "L%d" ret

let lbl_map = ref LabelMap.empty
let add_func func_name addr = lbl_map := LabelMap.add addr func_name !lbl_map
let add_lbl asm =
  let ret = match asm with
    | Je op -> op_to_string op
    | Jne op -> op_to_string op
    | Jge op -> op_to_string op
    | Jle op -> op_to_string op
    | Jmp op -> op_to_string op
    | Call op -> op_to_string op 
    | _ -> "" in
  let lbl = if ret = "" then "" else get_lbl () in
  match ret with
  | "" -> ()
  | _ when LabelMap.mem ret !lbl_map -> ()
  | _ -> lbl_map := LabelMap.add ret lbl !lbl_map

let rec register_lbl asms =
  match asms with
  | [] -> ()
  | asm::_asms -> 
    let _ = add_lbl asm in
    register_lbl _asms

let replace_op op_str =
  if LabelMap.mem op_str !lbl_map then
    LabelMap.find op_str !lbl_map
  else
    op_str

let replace_from_asm asm =
  match asm with
  | Je (Label str) -> Je (Label (replace_op str))
  | Jne (Label str) -> Jne (Label (replace_op str))
  | Jle (Label str) -> Jle (Label (replace_op str))
  | Jge (Label str) -> Jge (Label (replace_op str))
  | Jmp (Label str) -> Jmp (Label (replace_op str))
  | Call (Label str) -> Call (FuncLabel (replace_op str))
  | _ -> asm

let insert_lbl asms addrs =
  let _ = register_lbl asms in
  let asms_rev = List.rev asms in
  let addrs_rev = List.rev addrs in
  let rec insert_lbl asm addrs ret =
    match (asm, addrs) with
    | ([], []) -> ret
    | (asm::_asms, []) -> ret
    | ([], addr::_addrs) -> ret
    | (asm::_asms, addr::_addrs) -> 
      if LabelMap.mem addr !lbl_map then
        let lbl = Label (Label (LabelMap.find addr !lbl_map)) in
        let _asm = replace_from_asm asm in
        insert_lbl _asms _addrs (lbl::_asm::ret)
      else
        let _asm = replace_from_asm asm in
        insert_lbl _asms _addrs (_asm::ret) in
  insert_lbl asms_rev addrs_rev []

