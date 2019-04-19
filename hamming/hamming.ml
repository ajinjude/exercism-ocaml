open Base
type nucleotide = A | C | G | T

let equal (x,y) =
  match (x,y) with
  | (A,A) -> true
  | (C,C) -> true
  | (G,G) -> true
  | (T,T) -> true
  | _ -> false

let hamming_distance strand1 strand2 =
  match List.zip strand1 strand2 with
  | None -> None
  | Some xs -> Some (List.count ~f:(fun x -> not(equal x)) xs)