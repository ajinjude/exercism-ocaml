open Base
let sum = List.fold ~f: (+) ~init: 0

let square_of_sum n =
  let x = sum (List.range 1 (n + 1)) in
  x * x

let sum_of_squares n =
  let squares = List.map ~f: (fun x -> x * x) (List.range 1 (n + 1)) in
  sum squares

let difference_of_squares n = square_of_sum n - sum_of_squares n