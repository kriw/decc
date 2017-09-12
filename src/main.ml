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
      let restored = ControlFlow.restore_control_flow ctl_flow in
      let func = Backend.emit_func k restored in
      let _ = print_endline func in
      ()
    ) codes
