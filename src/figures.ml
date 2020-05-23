open Base
open Math

let semi_solid = "."
let solid = "*"
let empty = " "

let cast_rect width height ~f =
  let rec loop x y acc =
    if y = height then
      acc
    else if x = width then
      loop 0 (y + 1) (acc ^ "\n")
    else begin
      let rendered_value = f x y in
      loop (x + 1) y (acc ^ rendered_value)
    end
  in
  loop 0 0 ""

let line width height depth =
  let f _ y =
    if y <= depth then
      solid
    else
      empty
  in
  cast_rect width height ~f

let screen width height =
  let f _ y =
    if y % 2 = 0 then
      solid
    else
      semi_solid
  in
  cast_rect width height ~f

let rect width height buffer =
  let f x y =
    if x >= buffer && y >= buffer && x < (width - buffer) && y <= (height - 1 - buffer) then
      solid
    else
      empty
  in
  cast_rect width height ~f

let circle width height radius =
  let center = (width / 2, height / 2) in
  let f x y =
    if dist_sq (x, y) center < square radius then
      solid
    else
      empty
  in
  cast_rect width height ~f

