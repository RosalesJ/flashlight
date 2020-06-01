open ANSITerminal
open Lib

let clear_canvas () =
  erase Screen;
  set_cursor 1 1

let render frame =
  clear_canvas ();
  (* This allows it to render properly *)
  let frame = String.sub frame 0 ((String.length frame) - 1) in
  Stdio.printf "%s" frame;
  Stdlib.flush Stdio.stdout

let start_canvas () =
  ignore (Sys.command "tput smcup")

let end_canvas () =
  ignore (Sys.command "tput rmcup")

let init () =
  let width, height = size () in
  Camera.from_terminal width height

let _cam_test camera =
  let width, height = size () in
  let line = Figures.line camera 500. width height; in
  ignore line;
  Stdio.printf "%s\n" (Camera.show camera);
  Stdio.printf "%d %d\n" width height

let render_test camera =
  start_canvas ();
  let width, height = size () in
  let line = Figures.circle camera 350. width height; in
  render line;
  ignore (Unix.sleep 5);
  end_canvas ()


let () =
  let camera = init () in
  render_test camera
