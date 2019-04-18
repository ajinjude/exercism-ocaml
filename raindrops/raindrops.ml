let raindrop num =
  let check n = num % n = 0 in
  match (check 3, check 5, check 7) with
  | (true,true,true) -> "PlingPlangPlong"
  | (true,true,_) -> "PlingPlang"
  | (true,_,true) -> "PlingPlong"
  | (_,true,true) -> "PlangPlong"
  | (true,_,_) -> "Pling"
  | (_,true,_) -> "Plang"
  | (_,_,true) -> "Plong"
  | _ -> Int.to_string num
