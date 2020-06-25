open ANSITerminal
open Lib
open Render
open Base

let init () =
  let width, height = size () in
  Camera.from_terminal width height ~zoom:0.004

let _cam_test camera =
  Stdio.printf "%s\n" (Camera.show camera)

let _plane_b = Figure.Plane.{ origin = (Point3.make ~y:(-5.) ()); normal = (Camera.up) }
let plane_d = Figure.Plane.{ origin = (Point3.make ~x:(-6.) ~y:(-2.) ()); normal = (Point3.unit (Point3.make ~x:1. ~y:1. ())) }
let _plane_dz = Figure.Plane.{ origin = (Point3.make ~x:(-3.) ~y:(-2.) ()); normal = (Point3.unit (Point3.make ~x:1. ~y:1. ~z:(-1.)())) }

let _sphere = Figure.Sphere.{ center = (Point3.make ~z:(10.) ~x:(-2.) ()); radius = 6. }
let _sphere2 = Figure.Sphere.{ center = (Point3.make ~z:(4.) ~x:6. ()); radius = 2. }

let render_scene camera duration scene =
  start_canvas ();
  scene
  |> cast_cam ~camera ~duration
  |> Frame.render;
  end_canvas ()
    
let _translate_spheres =
  let rec loop n acc =
    if n = 0 then
      acc
    else
      loop (n - 1)
        (Figure.Sphere.{ center = (Point3.make ~z:(8.5 +. (Float.of_int n) /. 3.) ()); radius = 8. }
         :: acc)
  in
  let scene = loop 30 [] in
  Scene.of_figures (module Figure.Sphere) scene

let _plain_test camera =
  let _plane_v = Figure.Plane.{ origin = (Point3.make ~x:(-5.) ()); normal = (Camera.right camera)} in
  (* [_plane_b; _plane_v; (\* plane_d; plane_dz *\)] *)
  Scene.empty
  |> Scene.insert (module Figure.Plane) _plane_b
  |> Scene.insert (module Figure.Plane) _plane_v
  (* |> Scene.insert (module Figure.Plane) _plane_dz *)
  |> Scene.insert (module Figure.Sphere) _sphere
  |> Scene.insert (module Figure.Sphere) _sphere2

let _triangle_test=
  let triangle = Figure.Triangle.{
      b = Point3.make ~x:(-6.) ~y:(2.) ~z:1. ();
      c = Point3.make ~x:(4.) ~y:(-3.) ~z:5. ();
      a = Point3.make ~x:(11.) ~y:(7.) ~z:10. ()
    }
  in
  Scene.insert (module Figure.Triangle) triangle Scene.empty

let _square_test =
  let triangle = Figure.Square.{
      a = Point3.make ~x:(-1.) ~y:(1.) ~z:1. ();
      c = Point3.make ~x:(1.) ~y:(1.) ~z:1. ();
      b = Point3.make ~x:(-1.) ~y:(-1.) ~z:1. ();
      d = Point3.make ~x:(1.) ~y:(-1.) ~z:1. ()
    }
  in
  Scene.insert (module Figure.Square) triangle Scene.empty

let make_cube center r =
  let open Point3 in
  let (x, y, z) = tuple center in

  let a = make ~x:(x -. r) ~y:(y +. r) ~z:(z +. r) () in
  let b = make ~x:(x +. r) ~y:(y +. r) ~z:(z +. r) () in
  let c = make ~x:(x -. r) ~y:(y +. r) ~z:(z -. r) () in
  let d = make ~x:(x +. r) ~y:(y +. r) ~z:(z -. r) () in
  let h = make ~x:(x -. r) ~y:(y -. r) ~z:(z +. r) () in
  let e = make ~x:(x +. r) ~y:(y -. r) ~z:(z +. r) () in
  let f = make ~x:(x -. r) ~y:(y -. r) ~z:(z -. r) () in
  let g = make ~x:(x +. r) ~y:(y -. r) ~z:(z -. r) () in

  Figure.Cube.{
      up = Figure.make_square ~a ~b ~c ~d;
      down = Figure.make_square ~a:h ~b:e ~c:f ~d:g;
      left = Figure.make_square ~a ~b:h ~c ~d:f;
      right = Figure.make_square ~a:e ~b ~c:g ~d;
      front = Figure.make_square ~a:f ~b:g ~c ~d;
      back = Figure.make_square ~a ~b ~c:h ~d:e
    } 

let _cube_test =
  let center = Point3.make ~x:(-3.5) ~y:(-4.5) ~z:(5.) () in
  let r = 3. in
  let cube = make_cube center r in

  Scene.insert (module Figure.Cube) cube Scene.empty

let _final_test camera =
  [Figure.Sphere.{ center = (Point3.make ~z:10. ()); radius = 5. }]
  |> Scene.of_figures (module Figure.Sphere)
  |> Render.cast_cam ~camera ~duration:3.
  |> ignore


let () =
  let camera = init () in
  let duration = 5. in
  (* _plain_test camera; *)
  (* _render_test camera; *)
  (* _cam_test camera; *)
  (* _final_test camera; *)
  _cam_test camera;
  (* _triangle_test *)
  (* |> cast_cam ~camera ~duration *)
  (* |> ignore; *)
  _cube_test
  |> render_scene camera duration;
  ignore ()

