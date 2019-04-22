
open Base

let bottles = function
  | 0 -> "no more bottles"
  | 1 -> "1 bottle"
  | x -> Int.to_string x ^ " bottles"

let on_wall x =
  match x with
  | 0 -> String.capitalize (bottles x) ^ " of beer on the wall, " ^ bottles x ^ " of beer.\n"
  | _ -> bottles x ^ " of beer on the wall, " ^ bottles x ^ " of beer.\n"

let action x =
  let it = if x = 1 then "it" else "one" in
  match x with
  | 0 -> "Go to the store and buy some more, 99 bottles of beer on the wall.\n"
  | _ -> "Take " ^ it ^ " down and pass it around, " ^ bottles (x - 1) ^ " of beer on the wall.\n"


let verse x = on_wall x ^ action x

let lyrics ~from ~until =
  let range = List.range until (from + 1) in
  List.rev_map range ~f: verse |> String.concat ~sep: "\n"