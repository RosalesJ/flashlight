open Base

type t = { position: Point3.t; intersect: Ray.t -> string }

let blk = "▓"
let semi_solid = "."
let solid = "*"
let empty = " "

let line depth =
  let open Float in
  let intersect Ray.{origin=Point3.{y; _}; _} =
    if y <= depth then
      blk
    else
      empty
  in
  { position = Point3.origin; intersect }

let screen =
  let intersect Ray.{origin=Point3.{y; _}; _} =
    if (Int.of_float y) % 2 = 0 then
      semi_solid
    else
      solid
  in
  { position = Point3.origin; intersect }

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

let circle ?(center = Point3.origin) ~r =
  let open Float in
  let intersect Ray.{origin=Point3.{x; y; _}; _} =
    if Point3.dist Point3.{x; y; z = 0.} center < r then
      solid
    else
      empty
  in
  { position=center; intersect }
