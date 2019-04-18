open Base

let factors num = List.range 1 (num + 1)|> List.filter ~f: (fun n -> num % n = 0 )

let includes int_list n = List.exists ~f:(fun x -> x = n) int_list

let raindrop num =
  let factors = factors num in
  let check = includes factors in
  match (check 3, check 5, check 7) with
  | (true,true,true) -> "PlingPlangPlong"
  | (true,true,_) -> "PlingPlang"
  | (true,_,true) -> "PlingPlong"
  | (_,true,true) -> "PlangPlong"
  | (true,_,_) -> "Pling"
  | (_,true,_) -> "Plang"
  | (_,_,true) -> "Plong"
  | _ -> Int.to_string num
