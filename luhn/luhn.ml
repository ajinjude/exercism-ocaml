open Base

let checksum str =
  let doubler x = let double = x + x in if double > 9 then double - 9 else double in
  let num = String.to_list  str |> List.rev_map ~f: (fun x -> Char.to_int x - 48)  in
  List.mapi ~f: (fun x y -> if x % 2 = 0 then y else doubler y ) num 
  |> List.fold ~init: 0 ~f:(+)
  
let valid number =
  let str = String.filter ~f: (fun x -> not(Char.is_whitespace x )) number in
  String.length str > 1 && (String.for_all ~f: Char.is_digit str) && checksum str % 10 = 0