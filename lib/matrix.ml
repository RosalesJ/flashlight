open Base

type t = {
  a0: float; b0: float; c0: float;
  a1: float; b1: float; c1: float;
  a2: float; b2: float; c2: float
}

let t_dot (a1, b1, c1) (a2, b2, c2) = a1 *. a2 +. b1 *. b2 +. c1 *. c2

let transpose { a0; b0; c0; a1; b1; c1; a2; b2; c2 } =
  {a0= a0; b0=a1; c0=a2;
   a1= b0; b1=b1; c1=b2;
   a2= c0; b2=c1; c2=c2
  }

let apply { a0; b0; c0; a1; b1; c1; a2; b2; c2} p =
  let p = Point3.tuple p in
  Point3.make ~xyz:((t_dot (a0, b0, c0) p),
                    (t_dot (a1, b1, c1) p),
                    (t_dot (a2, b2, c2) p))

let map { a0; b0; c0; a1; b1; c1; a2; b2; c2} ~f =
  { a0 = f a0; b0 = f b0; c0 = f c0; a1 = f a1; b1 = f b1; c1 = f c1; a2 = f a2; b2 = f b2; c2 = f c2 }

let scale ~c a =
  map ~f:(( *.) c) a

let compose
    { a0; b0; c0; a1; b1; c1; a2; b2; c2 }
    { a0=ax; b0=ay; c0=az; a1=bx; b1=by; c1=bz; a2=cx; b2=cy; c2=cz} =
  { a0 = t_dot (a0, b0, c0) (ax, bx, cx);
    b0 = t_dot (a0, b0, c0) (ay, by, cy);
    c0 = t_dot (a0, b0, c0) (az, bz, cz);
    a1 = t_dot (a1, b1, c1) (ax, bx, cx);
    b1 = t_dot (a1, b1, c1) (ay, by, cy);
    c1 = t_dot (a1, b1, c1) (az, bz, cz);
    a2 = t_dot (a2, b2, c2) (ax, bx, cx);
    b2 = t_dot (a2, b2, c2) (ay, by, cy);
    c2 = t_dot (a2, b2, c2) (az, bz, cz) }

let id = { a0=1.; b0=0.; c0=0.;
           a1=0.; b1=1.; c1=0.;
           a2=0.; b2=0.; c2=1. }

let from_basis ~i ~j ~k =
  let (a0, a1, a2) = Point3.tuple i in
  let (b0, b1, b2) = Point3.tuple j in
  let (c0, c1, c2) = Point3.tuple k in
  { a0; b0; c0
  ; a1; b1; c1
  ; a2; b2; c2 }


let adj { a0=a; b0=b; c0=c; a1=d; b1=e; c1=f; a2=g; b2=h; c2=i} =
    let a0 =   (e *. i -. f *. h) in
    let b0 = -.(d *. i -. f *. g) in
    let c0 =   (d *. h -. e *. g) in
    let a1 = -.(b *. i -. c *. h) in
    let b1 =   (a *. i -. c *. g) in
    let c1 = -.(a *. h -. b *. g) in
    let a2 =   (b *. f -. c *. e) in
    let b2 = -.(a *. f -. c *. d) in
    let c2 =   (a *. e -. b *. d) in
    { a0; b0; c0
    ; a1; b1; c1
    ; a2; b2; c2 }

let det_adj {a0=a; b0=b; c0=c; _} {a0; b0; c0; _} =
  a *. a0 +. b *. b0 +. c *. c0

let det a = adj a |> det_adj a

let inv_exn a =
  let c = 1. /. det a in
  adj a
  |> transpose
  |> scale ~c

let inv a =
  try
    Some (inv_exn a)
  with Division_by_zero ->
    None
      
let make ((a0, b0, c0), (a1, b1, c1), (a2, b2, c2)) =
      { a0; b0; c0; a1; b1; c1; a2; b2; c2 }

let show { a0; b0; c0; a1; b1; c1; a2; b2; c2 } =
  Printf.sprintf "| %0.2f %0.2f %0.2f |\n| %0.2f %0.2f %0.2f |\n| %0.2f %0.2f %0.2f |"
    a0 b0 c0
    a1 b1 c1
    a2 b2 c2
