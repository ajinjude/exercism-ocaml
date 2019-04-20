open Base
open Base.Poly

let clean str =
let is_bracket x = x = '(' || x = '[' || x = '{' || x = ')' || x = ']' || x = '}' in
  String.filter ~f: is_bracket str |> String.to_list

let pop bracket stack =
  match stack with
  | x::xs when x = bracket -> xs
  | _ -> bracket::stack

let are_balanced str =
  let rec aux stack char_list =
    match char_list with
    | [] -> stack
    | x::xs ->  match x with
                | '(' | '{' | '[' -> aux (x::stack) xs
                | ')' -> aux (pop '(' stack) xs
                | ']' -> aux (pop '[' stack) xs
                | '}' -> aux (pop '{' stack) xs
                | x -> x::stack
  in
  List.is_empty (aux ([]) (clean str))