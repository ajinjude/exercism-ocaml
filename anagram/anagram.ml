open Base
let sorted_list str = String.lowercase str 
                      |> String.to_list
                      |> List.sort ~compare: Char.compare

let anagrams str strings =
  let not_equal x y = String.(String.lowercase x <> String.lowercase y) in
  let deduplicated_lst = List.filter ~f: (not_equal str) strings in
  let str = sorted_list str in
  List.filter ~f: (fun x -> (List.equal (sorted_list x) str ~equal:Char.equal)) deduplicated_lst
