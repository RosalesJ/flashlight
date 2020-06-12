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
    a = Point3.make ~x:(-0.5) ~y:(-1.2) ~z:2.3 ();
    b = Point3.make ~x:(0.5) ~y:(-1.2) ~z:2.3 ();
    c = Point3.make ~x:(0.) ~y:(0.5) ~z:3. ()
  }
in
Scene.insert (module Figure.Triangle) triangle Scene.empty

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
  _triangle_test
  (* |> cast_cam ~camera ~duration *)
  (* |> ignore; *)
  |> render_scene camera duration;
  ignore ()

