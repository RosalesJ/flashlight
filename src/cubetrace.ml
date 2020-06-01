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

let cam_test camera =
  let width, height = size () in
  render (Camera.show camera);
  Unix.sleep 3;
  render @@ Figures.line camera 200. width height;
  Unix.sleep 4
    

let () =
  let camera = init () in

  start_canvas ();

  cam_test camera;
  
  end_canvas ()
