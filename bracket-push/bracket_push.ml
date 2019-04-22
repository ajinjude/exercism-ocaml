open Base

let clean str =
  let is_bracket = function '(' | '[' | '{' | ')' | ']' | '}' -> true | _ -> false in 
  String.filter ~f: is_bracket str |> String.to_list

let pop bracket = function
  | x::xs when Char.(x = bracket) -> xs
  | _ -> [bracket]

let left = function ')' -> '(' | ']' -> '[' | '}' -> '{' | x -> x

let are_balanced str =
  let rec aux stack  = function
    | [] -> stack
    | x::xs ->  match x with
                | '(' | '{' | '[' -> aux (x::stack) xs
                | ')' | ']' | '}' as x -> aux (pop (left x) stack) xs
                | x -> x::stack
  in
  List.is_empty (aux [] (clean str))