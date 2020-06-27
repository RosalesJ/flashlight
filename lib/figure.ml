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
  val move : Affine.t -> t -> t
end

module Sphere : sig
  type t = { center: Point3.t
           ; radius: float
           }
  include Movable with type t := t
end = struct
  open Point3
  type t = {center: Point3.t; radius: float}

  let move trans sphere =
    let transformed = Affine.apply trans sphere.center in
    { sphere with center = transformed }

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
      |> Matrix.(apply @@ inv_exn @@ from_basis ~i:ab ~j:ac ~k:normal)
      |> tuple
      |> function (x, y, _) ->
        if x >= 0. && y >= 0. && x +. y <= 1. then
          dist
        else
          Float.infinity
    end

  let move trans {a; b; c} =
    let update = Affine.apply trans in
    {a= update a; b= update b; c= update c}
end


module Square : sig
  type t = {a : Point3.t; b: Point3.t; c: Point3.t; d: Point3.t}
  include Movable with type t := t
end = struct
  type t = {a : Point3.t; b: Point3.t; c: Point3.t; d: Point3.t}

  let pointwise f {a; b; c; d} = {a=f a; b=f b; c=f c; d=f d}

  let intersect ray {a; b; c; d} =
    let t1 = Triangle.{a; b; c} in
    let t2 = Triangle.{a=d; b; c} in
    min (Triangle.intersect ray t1) (Triangle.intersect ray t2)

  let move trans sq = pointwise (Affine.apply trans) sq
end

module Cube : sig
  type t =
    { front: Square.t
    ; back: Square.t
    ; up: Square.t
    ; down: Square.t
    ; left: Square.t
    ; right: Square.t }
  include Movable with type t := t
end = struct
  type t =
    { front: Square.t
    ; back: Square.t
    ; up: Square.t
    ; down: Square.t
    ; left: Square.t
    ; right: Square.t }

  let pointwise f {up; back; left; right; front; down} =
    {up = f up; back = f back; left = f left; right = f right; front = f front; down = f down }

  let intersect ray {front; back; up; down; left; right} =
    [front; back; up; down; left; right]
    |> List.map (Square.intersect ray)
    |> List.fold_left min Float.infinity

  let move trans cube = pointwise (Square.move trans) cube
end

let make_square ~a ~b ~c ~d =
  Square.{a; b; c; d}


let make_cube center r =
  let open Point3 in
  let (x, y, z) = tuple center in

  let a = make ~xyz:(x -. r, y +. r, z +. r) in
  let b = make ~xyz:(x +. r, y +. r, z +. r) in
  let c = make ~xyz:(x -. r, y +. r, z -. r) in
  let d = make ~xyz:(x +. r, y +. r, z -. r) in
  let h = make ~xyz:(x -. r, y -. r, z +. r) in
  let e = make ~xyz:(x +. r, y -. r, z +. r) in
  let f = make ~xyz:(x -. r, y -. r, z -. r) in
  let g = make ~xyz:(x +. r, y -. r, z -. r) in

  Cube.{
    up    = make_square ~a   ~b   ~c   ~d;
    back  = make_square ~a   ~b   ~c:h ~d:e;
    left  = make_square ~a   ~b:h ~c   ~d:f;
    right = make_square ~a:e ~b   ~c:g ~d;
    front = make_square ~a:f ~b:g ~c   ~d;
    down  = make_square ~a:h ~b:e ~c:f ~d:g;
  }

let triangle_center Triangle.{a; b; c} =
  let (ax, ay, az) = Point3.tuple a in
  let (bx, by, bz) = Point3.tuple b in
  let (cx, cy, cz) = Point3.tuple c in
  Point3.make ~xyz:(
    (ax +. bx +. cx) /. 3.,
    (ay +. by +. cy) /. 3.,
    (az +. bz +. cz) /. 3.)
