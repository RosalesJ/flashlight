
type t = { position: Point3.t; intersect: Ray.t -> float }

let blk = 0.
let solid = 1.
let semi_solid = 2.
let empty = 3.

let line depth =
  let intersect Ray.{origin=Point3.{y; _}; _} =
    if y <= depth then
      blk
    else
      empty
  in
  { position = Point3.origin; intersect }

let screen =
  let intersect Ray.{origin=Point3.{y; _}; _} =
    if (Int.of_float y) mod 2 = 0 then
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
  let intersect Ray.{origin=Point3.{x; y; _}; _} =
    if Point3.dist Point3.{x; y; z = 0.} center < r then
      solid
    else
      empty
  in
  { position=center; intersect }

let sphere ?(center = Point3.origin) ~r =
  let open Point3 in
  let intersect Ray.{ origin=p; direction=d } =
    let diff = p <+> neg center in
    let comp = dot d diff in
    let discr = (comp *. comp) -. (dot diff diff) +. (r *. r) in
    if discr < 0. then
      Float.infinity
    else begin      
      let plus = -. comp +. sqrt discr in
      let minus = -. comp -. sqrt discr in
      (* Stdio.printf "Origin:%s  Dir:%s\n" (Point3.show p) (Point3.show d);
       * Stdio.printf "  Diff:%s  Comp:%0.3f  discr: %0.3f.  plus:%0.3f  minus: %0.3f\n" (Point3.show diff) comp discr plus minus; *)
      Float.min plus minus
    end
  in

  {position=center; intersect}
