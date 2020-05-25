open ANSITerminal

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

let _temp_render () =
  Stdio.print_endline "temp_message lol"

let init () =
  let width, height = size () in
  Camera.from_terminal width height ~zoom:1. ~center:(0., 0.)

let cam_test camera =
  render "hello ";
  Unix.sleep 1;
  render (Camera.show camera);
  Unix.sleep 2;
  render (Figures.circle camera 600.);
  Unix.sleep 5

let () =
  let camera = init () in

  start_canvas ();

  cam_test camera;
  
  end_canvas ()
