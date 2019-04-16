let is_triangle x y z =
  if x = 0 || y = 0 || z = 0
  then false
  else x + y > z && y + z > x && z + x > y

let is_equilateral x y z = is_triangle x y z && x = y && y = z 

let is_isosceles x y z = is_triangle x y z && (x = y || y = z || z = x)

let is_scalene x y z = is_triangle x y z && (x <> y && y <> z && z <> x)