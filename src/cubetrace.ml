open ANSITerminal
open Lib
open Render
open Base

let init () =
  let width, height = size () in
  Camera.from_terminal width height ~zoom:0.004

let _cam_test camera =
  Stdio.printf "%s\n" (Camera.show camera)

let _plane_b = Figure.Plane.{ origin = (Point3.make ~y:(-. 3.) ()); normal = (Camera.up) }
let plane_d = Figure.Plane.{ origin = (Point3.make ~x:(-. 3.) ~y:(-2.) ()); normal = (Point3.unit (Point3.make ~x:1. ~y:1. ())) }
let _plane_dz = Figure.Plane.{ origin = (Point3.make ~x:(-. 3.) ~y:(-2.) ()); normal = (Point3.unit (Point3.make ~x:1. ~y:1. ~z:(-. 1.)())) }
let _sphere = Figure.Sphere.{ center = (Point3.make ~z:(8.5) ()); radius = 6. }
    
let translate_spheres =
  let rec loop n acc =
    if n = 0 then
      acc
    else
      loop (n - 1)
        (Figure.Sphere.{ center = (Point3.make ~z:(8.5 +. (Float.of_int n) /. 3.) ()); radius = 8. }
         :: acc)
  in
  let scene = loop 30 [] in
  Scene.of_movable (module Figure.Sphere) scene

let _render_test camera =
  let duration = 5. in
  (* let line = Figure.line 0. in
   * let screen = Figure.screen in *)
  let frame = cast_cam ~camera ~duration translate_spheres in

  start_canvas ();
  
  Frame.render frame;

  end_canvas ()

let plain_test camera =
  let duration = 4. in
  let _plane_v = Figure.Plane.{ origin = (Point3.make ~x:(-. 4.) ()); normal = (Camera.right camera)} in
                   
  start_canvas ();

  [_plane_b; _plane_v; (* plane_d; plane_dz *)]
  |> Scene.of_non_movable (module Figure.Plane)
  |> Scene.insert_non_movable (module Figure.Plane) _plane_dz
  |> Scene.insert_movable (module Figure.Sphere) _sphere
  |> cast_cam ~camera ~duration
  |> Frame.render;
  
  end_canvas ()

let _final_test camera =
  [Figure.Sphere.{ center = (Point3.make ~z:10. ()); radius = 5. }]
  |> Scene.of_movable (module Figure.Sphere)
  |> Render.cast_cam ~camera ~duration:3.
  |> ignore


let () =
  let camera = init () in
  plain_test camera;
  (* _render_test camera; *)
  _cam_test camera;
  _final_test camera;
  ignore ()

