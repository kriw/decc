open ObjdumpParser
let _ = 
  let lines = load_file Sys.argv.(1) in
  let codes, func_addrs = parse_objdump lines "" StringMap.empty StringMap.empty in
  StringMap.iter (fun k v -> 
      let insns, addrs = parse v in
      let mnems, ops = insns in
      List.iter (fun x -> Util.print_list x) ops) codes
(* Util.print_list mnems) codes *)

