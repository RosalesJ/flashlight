type t = {
  a0: float; b0: float; c0: float;
  a1: float; b1: float; c1: float;
  a2: float; b2: float; c2: float
}

let t_dot (a1, b1, c1) (a2, b2, c2) = a1 *. a2 +. b1 *. b2 +. c1 *. c2

let apply p { a0; b0; c0; a1; b1; c1; a2; b2; c2} =
  let open Point3 in
  make
    ~x:(t_dot (a0, b0, c0) (x p, y p, z p))
    ~y:(t_dot (a1, b1, c1) (x p, y p, z p))
    ~z:(t_dot (a2, b2, c2) (x p, y p, z p))
    ()

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
