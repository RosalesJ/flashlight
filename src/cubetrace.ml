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
      loop (n - 1) (Figure.sphere ~center:(Point3.make ~z:(8. +. (Float.of_int n)) ()) ~r:8. :: acc)
  in
  loop 10 []

let render_test camera =
  let duration = 0.5 in
  (* let line = Figure.line 0. in
   * let screen = Figure.screen in *)

  start_canvas ();
  
  translate_spheres
  |> List.map ~f:(cast_cam ~camera ~duration)
  |> List.iter ~f:Frame.render;
  
  end_canvas ()

let _final_test camera =
  (Figure.sphere ~center:(Point3.make ~z:10. ()) ~r:5.)
  |> Render.cast_cam ~camera ~duration:0.5
  |> ignore


let () =
  let camera = init () in
  render_test camera;
  _cam_test camera;
  _final_test camera;
  ignore ()

