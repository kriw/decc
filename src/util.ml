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

let find_index f ls =
  let rec _find ls n =
    match ls with
    | [] -> n
    | x::xs when f x -> n
    | x::xs -> _find xs (n+1) in
  _find ls 0

let take ls n =
  let rec _take n ls ret =
    if n > 0 then
      match ls with
      | [] -> List.rev ret
      | x::xs -> _take (n-1) xs (x::ret)
    else List.rev ret in
  _take n ls []

let drop ls n =
  let rec _drop n ls =
    if n > 0 then
      match ls with
      | [] -> []
      | x::xs -> _drop (n-1) xs
    else ls in
  _drop n ls

