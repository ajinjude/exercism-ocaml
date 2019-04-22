open Base

let bottles = function
  | 0 -> "no more bottles"
  | 1 -> "1 bottle"
  | x -> Int.to_string x ^ " bottles"

let on_wall x =
  let bottle = bottles x in
  let bottles = if x = 0 then String.capitalize bottle else bottle in
  String.concat [bottles; " of beer on the wall, "; bottle; " of beer.\n"]

let action x =
  let it = if x = 1 then "it" else "one" in
  match x with
  | 0 -> "Go to the store and buy some more, 99 bottles of beer on the wall.\n"
  | _ -> String.concat ["Take "; it; " down and pass it around, "; bottles (x - 1); " of beer on the wall.\n"]

let verse x = on_wall x ^ action x

let lyrics ~from ~until = List.range until (from + 1) |> List.rev_map ~f: verse |> String.concat ~sep: "\n"
