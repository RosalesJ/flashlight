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

  let contains {a; b; c} p =
    let open Point3 in
    let ab = b <+> neg a in
    let ac = c <+> neg a in
    let ap = p <+> neg a in
    let normal = unit @@ cross ab ac in
    
    let basis_inv = Transform.inv_exn @@ Transform.from_basis ~i:ab ~j:ac ~k:normal in

    let transformed = Transform.apply basis_inv ap in
    let cond = match Point3.tuple transformed with
      | (x, y, _) -> x >= 0. && y >= 0. && x +. y <= 1.
    in cond

  let intersect ray { a; b; c } =
    let ab = b <+> neg a in
    let ac = c <+> neg a in
    let normal = cross ab ac in
    (* Stdio.printf "%s %s %s" (Point3.show ab) (Point3.show bc) (Point3.show normal); *)
    let pl = Plane.{origin=ab; normal} in
    let dist = Plane.intersect ray pl in
    (* Stdio.printf "%0.4f\n" dist; *)
    if dist >= Float.infinity then
      Float.infinity
    else begin
      let point = dist <*> ray.direction in
      if contains {a; b; c} point then
        dist
      else
        Float.infinity
    end

  let move _ p =
    {a=p; b=p; c=p}
end

let contains a b c p =
  let open Point3 in
  let ab = b <+> neg a in
  let ac = c <+> neg a in
  let ap = p <+> neg a in
  let normal = cross ab ac in

  let basis_inv = Transform.inv_exn @@ Transform.from_basis ~i:ab ~j:ac ~k:normal in

  let transformed = Transform.apply basis_inv ap in
  let repr = Point3.show (transformed) in
  let cond = match Point3.tuple transformed with
    | (x, y, _) -> x >= 0. && y >= 0. && x +. y <= 1.
  in (repr, cond)
