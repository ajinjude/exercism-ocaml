open Base
let clean_string x =
  String.lowercase x
  |> String.to_list
  |> List.filter ~f: Char.is_alpha
  |> List.dedup_and_sort ~compare: Char.compare
  |> String.of_char_list

let is_pangram x = 
  match clean_string x with
    | "abcdefghijklmnopqrstuvwxyz" -> true
    | _ -> false