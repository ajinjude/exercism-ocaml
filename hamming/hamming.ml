open Base
open Base.Poly

type nucleotide = A | C | G | T

let equal (x,y) = x = y

let hamming_distance strand1 strand2 =
  match List.zip strand1 strand2 with
  | None -> None
  | Some xs -> Some (List.count ~f:(Fn.non equal) xs)