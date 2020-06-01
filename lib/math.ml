open Base

let square x = x *. x

let dist_sq (x_1, y_1) (x_2, y_2) =
  (square (x_1 -. x_2)) +. (square (y_1 -. y_2))

let square x = x * x

let dist_sq_int (x_1, y_1) (x_2, y_2) =
  (square (x_1 - x_2)) + (square (y_1 - y_2))

let lt (x1, y1) (x2, y2) = x1 < x2 && y1 < y2

let eq (x1, y1) (x2, y2) = x1 = x2 && y1 = y2

