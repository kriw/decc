let print_list xs = print_endline (String.concat ", " xs)
let split delim s = Str.split (Str.regexp delim) s
let strip str = 
  let str = Str.replace_first (Str.regexp "^ +") "" str in
  Str.replace_first (Str.regexp " +$") "" str;;   
let unzip xy =
  let rec _unzip xys xs ys =
    match xys with
    | [] -> xs, ys
    | xy::_xys -> 
      let x, y = xy in
      _unzip _xys (List.append xs [x]) (List.append ys [y]); in
  _unzip xy [] []


