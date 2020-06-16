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
  type t = { center: Point3.t
           ; radius: float
           }
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


module Triangle : sig
  type t = { a: Point3.t; b: Point3.t; c: Point3.t }
  include Movable with type t := t
end = struct
  open Point3

  type t = { a: Point3.t; b: Point3.t; c: Point3.t }

  let intersect ray { a; b; c } =
    let ab = b <+> neg a in
    let ac = c <+> neg a in
    let normal = unit @@ cross ab ac in

    let plane = Plane.{origin=a; normal} in
    let dist = Plane.intersect ray plane in

    if dist >= Float.infinity then
      Float.infinity
    else begin
      dist <*> ray.direction <+> ray.origin <+> neg a
      |> Transform.(apply @@ inv_exn @@ from_basis ~i:ab ~j:ac ~k:normal)
      |> tuple
      |> function (x, y, _) ->
        if x >= 0. && y >= 0. && x +. y <= 1. then
          dist
        else
          Float.infinity
    end

  let move _ p =
    {a=p; b=p; c=p}
end
