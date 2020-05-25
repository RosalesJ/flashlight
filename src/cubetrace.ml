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

let () =
  let width, height = size () in
  let circle = Figures.circle width height 10 in
  let line = Figures.rect width height 4 in
  let camera = init () in

  start_canvas ();
  render (Camera.show camera);
  Unix.sleep 5;
  render "A rectangle of 3";
  Unix.sleep 1;
  render line;
  Unix.sleep 3;
  render "A rectangle of 5";
  Unix.sleep 1;
  render circle;
  Unix.sleep 1;
  
  end_canvas ()
