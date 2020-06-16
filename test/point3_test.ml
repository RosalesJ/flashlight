open Lib
open OUnit
open Point3

let i = make ~x:1. ()
let j = make ~y:1. ()
let k = make ~z:1. ()

let v = make ~z:(-. 1.) ~y:(2.) ()

let cross_test () =
  assert_equal (cross i j) k;
  assert_equal (cross k i) j;
  assert_equal (cross j k) i

let l2_test () =
  assert_equal (l2 i) 1.;
  assert_equal (l2 j) 1.;
  assert_equal (l2 k) 1.

let scale_test () =
  let test c p = assert_equal ((l2 p) *. c) @@ l2 (c <*> p) in
  test 2. i;
  test 3. v;
  test 2. j;
  test 5. k;
  test 10. v

let camera_test () =
  let open Camera in
  let cam = match from_terminal 7 5 with
    | {height; width; center; _} -> {height; width; center; normal=(Point3.make ~x:((sqrt 2.) /. 2.) ~z:((sqrt 2.) /. 2.) ()); resolution={x=10; y=40}; focus=(Point3.make ())}
  in
  let n = cam.normal in
  let r = right cam in
  assert_equal 1. (l2 r);
  assert_equal 1. (l2 n);
  assert_equal 1. (l2 up);
  assert (approx (cross up n) r);
  assert (approx (cross r up) n);
  assert (approx (cross n r) up)
  
let () =
  cross_test ();
  l2_test ();
  scale_test ();
  camera_test ()
