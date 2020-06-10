type t = { x: float; y: float; z: float }

let precision = 0.0000000001

let square x = x *. x

let sum_components {x; y; z} = x +. y +. z

let make ?(x = 0.) ?(y = 0.) ?(z = 0.) () = {x; y; z;}

let map { x; y; z } ~f = {x = f x; y = f y; z = f z}

let pointwise { x=x1; y=y1; z=z1 } { x=x2; y=y2; z=z2 } ~f =
  { x = f x1 x2
  ; y = f y1 y2
  ; z = f z1 z2 }
  
let l2 p = sqrt @@ sum_components @@ map ~f:square p

let dist p1 p2 = l2 @@ pointwise ~f:(-.) p1 p2

let add = pointwise ~f:(+.)

let scale p ~c = map ~f:(( *. ) c) p

let neg = map ~f:(fun x -> -.x)

let sub u v = add v (neg u)

let show {x; y; z} = Printf.sprintf "(%.3f, %.3f, %.3f)" x y z

let dot p1 p2 = sum_components @@ pointwise ~f:( *. ) p1 p2

let unit_vector p = scale ~c:(1. /. (sqrt @@ dot p p)) p

let _direction_vector u v = unit_vector @@ sub u v

let cross { x=a1; y=a2; z=a3 } { x=b1; y=b2; z=b3 } =
  { x = a2 *. b3 -. a3 *. b2
  ; y = a3 *. b1 -. a1 *. b3
  ; z = a1 *. b2 -. a2 *. b1 }

let ( <+> ) = add

let ( <*> ) c p = scale p ~c

let approx p1 p2 = (dist p1 p2) < precision

let origin = make ()

let unit p = 1. /. (l2 p) <*> p

let tuple {x; y; z} = (x, y, z)

let x p = p.x
let y p = p.y
let z p = p.z

