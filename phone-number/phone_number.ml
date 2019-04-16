open Base

let validate_N num = not Char.(num.[0] = '0' || num.[0] = '1' || num.[3] = '0' || num.[3] = '1')

let check_number num =
  if validate_N num
  then Some num
  else None

let number x =
  let num = String.filter ~f: Char.is_digit x in
  match String.length num with
  | 11 -> if Char.(num.[0] = '1')
          then check_number (String.drop_prefix num 1)
          else None
  | 10 -> check_number num 
  | _ -> None