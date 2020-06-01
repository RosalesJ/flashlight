open Base

let blk = "▓"
let semi_solid = "."
let solid = "*"
let empty = " "

let line cam depth =
  let open Float in
  let f Point3.{y; _} =
    if y <= depth then
      solid
    else
      semi_solid
  in
  Render.cast_cam cam ~f

let screen cam =
  let f Point3.{y; _} =
    if (Int.of_float y) % 2 = 0 then
      solid
    else
      semi_solid
  in
  Render.cast_cam cam ~f

(* let rect (cam : Camera.t) buffer =
 *   let open Float in
 *   let f Point3.{x; y; _} =
 *     if x >= (cam.x_min + buffer) &&
 *        y >= (cam.y_min + buffer) &&
 *        x < (cam.x_max - buffer) &&
 *        y <= (cam.y_max - 1. - buffer) then
 *       solid
 *     else
 *       empty
 *   in
 *   Render.cast_cam cam ~f *)

let circle (cam : Camera.t) radius =
  let open Float in
  let f Point3.{x; y; _} =
    if Point3.dist Point3.{x; y; z = 0.} cam.center < radius then
      solid
    else
      empty
  in
  Render.cast_cam cam ~f
