open Base
open Math

let blk = "▓"
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

let cast_cam (cam : Camera.t) x_step y_step ~f =
  let open Float in
  let rec loop x y acc =
    if y >= cam.y_max then
      acc
    else if x >= cam.x_max then
      loop cam.x_min (y + y_step) (acc ^ "\n")
    else begin
      let rendered_value = f x y in
      loop (x + x_step) y (acc ^ rendered_value)
    end
  in
  loop cam.x_min cam.y_min ""

let line cam depth =
  let open Float in
  let f _ y =
    if y <= depth then
      semi_solid
    else
      empty
  in
  cast_cam cam Camera.char_width Camera.char_height ~f

let screen cam =
  let f _ y =
    if (Int.of_float y) % 2 = 0 then
      solid
    else
      semi_solid
  in
  cast_cam cam Camera.char_width Camera.char_height ~f

let rect (cam : Camera.t) buffer =
  let open Float in
  let f x y =
    if x >= (cam.x_min + buffer) &&
       y >= (cam.y_min + buffer) &&
       x < (cam.x_max - buffer) &&
       y <= (cam.y_max - 1. - buffer) then
      solid
    else
      empty
  in
  cast_cam cam Camera.char_width Camera.char_height ~f

let circle cam radius =
  let open Float in
  let center = Camera.center cam in
  let f x y =
    if dist_sq (x, y) center < square radius then
      solid
    else
      empty
  in
  cast_cam cam Camera.char_width Camera.char_height ~f
