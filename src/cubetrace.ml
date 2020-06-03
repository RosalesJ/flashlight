open ANSITerminal
open Lib
open Render
open Base

let init () =
  let width, height = size () in
  Camera.from_terminal width height ~zoom:0.004

let _cam_test camera =
  Stdio.printf "%s\n" (Camera.show camera)

let translate_spheres =
  let rec loop n acc =
    if n = 0 then
      acc
    else
      loop (n - 1) (Figure.sphere ~center:(Point3.make ~z:(8.5 +. (Float.of_int n) /. 3.) ()) ~r:8. :: acc)
  in
  loop 30 []

let _render_test camera =
  let duration = 0.013 in
  (* let line = Figure.line 0. in
   * let screen = Figure.screen in *)
  let frames =   translate_spheres
                 |> List.map ~f:(cast_cam ~camera ~duration)
  in

  start_canvas ();
  
  frames
  |> List.iter ~f:Frame.render;

  end_canvas ()

let plain_test camera =
  let duration = 2. in
  (* let line = Figure.line 0. in
   * let screen = Figure.screen in *)

  let plane_b = Figure.plane ~origin:(Point3.make ~y:(-. 2.) ()) ~normal:(Camera.up) in
  let plane_v = Figure.plane ~origin:(Point3.make ~x:(-. 4.) ()) ~normal:(Camera.right camera) in
  let plane_d = Figure.plane ~origin:(Point3.make ~x:(-. 3.) ~y:(-2.) ()) ~normal:(Point3.unit (Point3.make ~x:1. ~y:1. ())) in
  let plane_dz = Figure.plane ~origin:(Point3.make ~x:(-. 3.) ~y:(-2.) ()) ~normal:(Point3.unit (Point3.make ~x:1. ~y:1. ~z:(-. 1.)())) in

  start_canvas ();

  [plane_b; plane_v; plane_d; plane_dz]
  |> List.map ~f:(cast_cam ~camera ~duration)
  |> List.iter ~f:Frame.render;
  
  end_canvas ()

let _final_test camera =
  (Figure.sphere ~center:(Point3.make ~z:10. ()) ~r:5.)
  |> Render.cast_cam ~camera ~duration:0.5
  |> ignore


let () =
  let camera = init () in
  plain_test camera;
  _cam_test camera;
  _final_test camera;
  ignore ()

