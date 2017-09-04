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

let extract_header line = Str.global_replace header_regex "\\1,\\2" line

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
    let addrFunc = Str.split (Str.regexp_string ",") (extract_header head) in
    let addr = List.nth addrFunc 0 in
    let func = List.nth addrFunc 1 in
    parse_objdump tail func codes (StringMap.add func addr addrs)
  | head :: tail -> 
    parse_objdump tail funcName (append funcName head codes) addrs

let print_list = List.iter (fun s -> print_endline s)

let _ = 
  let lines = load_file Sys.argv.(1) in
  let codes, addrs = parse_objdump lines "" StringMap.empty StringMap.empty in
  StringMap.iter (fun k v -> print_endline k; print_list v) codes
