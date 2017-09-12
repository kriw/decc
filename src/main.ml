open ObjdumpParser

let _ = 
  let lines = load_file Sys.argv.(1) in
  let codes, func_addrs = parse_objdump lines "" StringMap.empty StringMap.empty in
  let _ = StringMap.iter (fun func_name addr -> Asm.add_func func_name addr) func_addrs in
  StringMap.iter (fun k v -> 
      let insns, _addrs = parse v in
      let addrs = List.map (Str.global_replace (Str.regexp " \\([0-9a-fA-F]+\\):") "\\1") _addrs in
      let mnems, ops = insns in
      let asms = List.map (fun (x, y) -> Asm.to_asm x y) (List.combine mnems ops) in
      let asms_with_lbl = Asm.insert_lbl asms addrs in
      let asts = Ast.to_ast asms_with_lbl in
      let ctl_flow = ControlFlow.from_ast asts in
      let with_if = ControlFlow.restore_if ctl_flow in
      List.iter print_endline (List.map ControlFlow.to_string with_if)
      (* let func = Backend.emit_func k asts in *)
      (* print_endline func *)
      (* List.iter (fun x -> print_endline (Ast.to_string x)) asts *)
      (* List.iter (fun x -> Ast.print_ast x) asts *)
      (* print_endline k; List.iter (fun asm -> print_endline (Asm.to_string asm)) asms *)
      (* print_endline k; List.iter (fun asm -> print_endline (Asm.to_string asm)) asms_with_lbl *)
      (* print_endline k; List.iter (fun (asm, addr) -> Printf.printf "%s %s\n" addr (Asm.to_string asm)) (List.combine asms addrs) *)
    ) codes
