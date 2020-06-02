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

let blk = "▓"
let semi_solid = "."
let solid = "*"
let empty = " "

let char_from_distance = function
  | x when x >= 0. && x < 1. -> blk
  | x when x >= 1. && x < 2. -> solid
  | x when x >= 2. && x < 3. -> semi_solid
  | _ -> empty

let cast_cam ~(camera: Camera.t) ~duration figure =
  let open Point3 in
  let horiz_step = camera.width /. (Float.of_int camera.resolution.x) <*> Camera.right camera  in
  let vert_step = camera.height /. (Float.of_int camera.resolution.y) <*> Camera.up in

  let init =
    let x = -. camera.height /. 2. <*> Camera.up in
    let y = -. camera.width /. 2. <*> Camera.right camera in
    let char_off_x = Camera.char_width /. 2. <*> Camera.right camera in
    let char_off_y = Camera.char_height /. 2. <*> Camera.up in
    camera.center <+> x <+> y <+> char_off_x <+> char_off_y
  in

  let rec loop x y acc =
    if y = camera.resolution.y then
      acc
    else if x = camera.resolution.x then
      loop 0 (y + 1) (acc ^ "\n")
    else begin
      let sample_point = init
                         <+> (Float.of_int x <*> horiz_step)
                         <+> (Float.of_int y <*> vert_step)
      in
      let ray = Ray.{ origin=sample_point; direction = unit (sample_point <+> neg camera.focus) } in
      let dist = figure.intersect ray in
      loop (x + 1) y (acc ^ char_from_distance dist)
    end
  in
  Frame.{contents=loop 0 0 ""; duration }
