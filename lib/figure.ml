let blk = 0.
let solid = 1.
let semi_solid = 2.
let empty = 3.

module type T = sig
  type t
  val intersect: Ray.t -> t -> float
end

module type Movable = sig
  include T
  val move : t -> Point3.t -> t
end

module Sphere : sig
  type t = { center: Point3.t; radius: float }
  include Movable with type t := t
end = struct
  open Point3
  type t = {center: Point3.t; radius: float}

  let move sphere point =
    { sphere with center = point }

  let intersect Ray.{ origin=p; direction=d } sphere =
    let diff = p <+> neg sphere.center in
    let comp = dot d diff in
    let discr = (comp *. comp) -. (dot diff diff) +. (sphere.radius *. sphere.radius) in
    if discr < 0. then
      Float.infinity
    else begin
      let plus = -. comp +. sqrt discr in
      let minus = -. comp -. sqrt discr in
      Float.min plus minus
    end
end

module Plane : sig
  type t = { origin: Point3.t; normal: Point3.t }
  include T with type t := t
end = struct
  type t = { origin: Point3.t; normal: Point3.t }
  
  let intersect Ray.{ origin=p; direction=d } { origin; normal } =
      let dn = Point3.dot d normal in
      let pn = Point3.dot p normal in
    let on = Point3.dot origin normal in
    if dn == 0. then
      Float.infinity
    else begin
      let result = (on -.  pn) /. dn in
      if result < 0. then
        Float.infinity
      else
        result
    end
end


(* let line depth =
 *   let intersect Ray.{origin=Point3.{y; _}; _} =
 *     if y <= depth then
 *       blk
 *     else
 *       empty
 *   in
 *   { intersect }
 * 
 * let screen =
 *   let intersect Ray.{origin=Point3.{y; _}; _} =
 *     if (Int.of_float y) mod 2 = 0 then
 *       semi_solid
 *     else
 *       solid
 *   in
 *   { intersect } *)

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
(* let circle ?(center = Point3.origin) ~r =
 *   let intersect Ray.{origin=Point3.{x; y; _}; _} =
 *     if Point3.dist Point3.{x; y; z = 0.} center < r then
 *       solid
 *     else
 *       empty
 *   in { intersect } *)
