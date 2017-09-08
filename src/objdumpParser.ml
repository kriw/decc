open Printf

module StringMap = Map.Make(String);;

let load_file f =
  let ic = open_in f in
  let n = in_channel_length ic in
  let s = Bytes.create n in
  really_input ic s 0 n;
  close_in ic;
  Str.split (Str.regexp "\n") (Bytes.to_string s)

let header_regex = Str.regexp "\\([0-9a-fA-F]+\\) <\\(.+\\)>:"

let is_header line = Str.string_match header_regex line 0

let bool_to_string = function
  | true -> "true"
  | false -> "false"

let append k v m = 
  match StringMap.mem k m with
  | true -> let new_lst = List.append (StringMap.find k m) [v] in
    StringMap.add k new_lst m
  | false -> StringMap.add k [v] m

let rec parse_objdump lines funcName codes addrs =
  match lines with
  | [] -> (codes, addrs)
  | head :: tail when is_header head -> 
    let extract_header line = Str.global_replace header_regex "\\1,\\2" line in
    let addrFunc = Str.split (Str.regexp_string ",") (extract_header head) in
    let addr = List.nth addrFunc 0 in
    let func = List.nth addrFunc 1 in
    parse_objdump tail func codes (StringMap.add func addr addrs)
  | head :: tail when String.equal funcName "" -> 
    parse_objdump tail funcName codes addrs
  | head :: tail -> 
    parse_objdump tail funcName (append funcName head codes) addrs

let parse_insns insns = 
  let parse_insn insn = 
    let mnem, ops = match Util.split " " insn with
      | [] -> "", []
      | x::xs -> Util.strip x, Util.split "," (Util.strip (String.concat " " xs));in
    mnem, ops; in
  Util.unzip (List.map parse_insn insns)

let parse lines =
  let filtered = List.filter (fun x -> List.length (Util.split "\t" x) > 2) lines in
  let tmp = List.map (fun x -> 
      let s = Util.split "\t" x in
      (List.nth s 0), (List.nth s 2)
    ) filtered in
  let addrs, insns = Util.unzip tmp in
  parse_insns insns, addrs
