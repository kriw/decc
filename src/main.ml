open ObjdumpParser

let _ = 
  let lines = load_file Sys.argv.(1) in
  let codes, func_addrs = parse_objdump lines "" StringMap.empty StringMap.empty in
  StringMap.iter (fun k v -> 
      let insns, addrs = parse v in
      let mnems, ops = insns in
      let asms = List.map (fun (x, y) -> Asm.to_asm x y) (List.combine mnems ops) in
      let asts = Ast.to_ast asms in
      List.iter (fun x -> print_endline (Ast.to_string x)) asts
      (* print_endline k; List.iter (fun asm -> print_endline (Asm.to_string asm)) asms *)
    ) codes
