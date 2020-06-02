open ANSITerminal
open Figure

let clear_canvas () =
  erase Screen;
  set_cursor 1 1

let start_canvas () =
  ignore (Sys.command "tput smcup")

let end_canvas () =
  ignore (Sys.command "tput rmcup")

module Frame = struct
  type t = { contents: string; duration: float}

  let render {contents; duration} =
    clear_canvas ();
    (* This allows it to render properly *)
    let contents = String.sub contents 0 ((String.length contents) - 1) in
    Stdio.printf "%s" contents;
    Stdlib.flush Stdio.stdout;
    Unix.sleepf duration
end

let cast_cam ~(camera: Camera.t) x_samples y_samples ~figure =
  let open Point3 in
  let horiz_step = camera.width /. (Float.of_int x_samples) <*> Camera.right camera  in
  let vert_step = camera.height /. (Float.of_int y_samples) <*> Camera.up in

  let init =
    let x = -. camera.height /. 2. <*> Camera.up in
    let y = -. camera.width /. 2. <*> Camera.right camera in
    let char_off_x = Camera.char_width /. 2. <*> Camera.right camera in
    let char_off_y = Camera.char_height /. 2. <*> Camera.up in
    camera.center <+> x <+> y <+> char_off_x <+> char_off_y
  in

  let rec loop x y acc =
    if y = y_samples then
      acc
    else if x = x_samples then
      loop 0 (y + 1) (acc ^ "\n")
    else begin
      let sample_point = init
                         <+> (Float.of_int x <*> horiz_step)
                         <+> (Float.of_int y <*> vert_step)
      in
      let rendered_value = (figure: Figure.t).intersect Ray.{origin=sample_point; direction=camera.normal} in
      loop (x + 1) y (acc ^ rendered_value)
    end
  in
  Frame.{contents=loop 0 0 ""; duration = 3.}
