open ANSITerminal
open Lib
open Render
open Base

let init () =
  let width, height = size () in
  Camera.from_terminal width height

let _cam_test camera =
  let width, height = size () in
  let line = Figure.line 500. in
  ignore line;
  Stdio.printf "%s\n" (Camera.show camera);
  Stdio.printf "%d %d\n" width height

let render_test camera =
  let width, height = size () in
  let circle = Figure.circle ~center:Point3.origin ~r:500. in
  let line = Figure.line 0. in
  let screen = Figure.screen in

  start_canvas ();
  
  [line; circle; screen]
  |> List.map ~f:(fun figure -> Render.cast_cam ~camera ~figure width height)
  |> List.iter ~f:(Frame.render);
  
  end_canvas ()


let () =
  let camera = init () in
  render_test camera
