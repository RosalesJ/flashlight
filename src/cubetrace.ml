open ANSITerminal

let clear_canvas () =
  erase Screen;
  set_cursor 1 1

let render frame =
  clear_canvas ();
  Stdio.printf "%s" frame;
  Stdlib.flush Stdio.stdout;
  set_cursor 100 100

let start_canvas () =
  ignore (Sys.command "tput smcup")

let end_canvas () =
  ignore (Sys.command "tput rmcup")

let _temp_render () =
  Stdio.print_endline "temp_message lol"

let () =
  let width, height = size () in
  let circle = Figures.circle width height 10 in
  let line = Figures.rect width height 4 in
  start_canvas ();
  render "A rectangle of 3";
  Unix.sleep 1;
  render line;
  Unix.sleep 1;
  render "A rectangle of 5";
  Unix.sleep 1;
  render circle;
  Unix.sleep 1;
  
  end_canvas ()
