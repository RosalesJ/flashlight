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
let solid = "*"
let semi_solid = "o"
let semi_distant = "."
let distant = "`"
let empty = " "
let negative = "?"

let char_from_distance = function
  | x when x < 0. -> negative
  | x when x >= 0. && x < 3. -> blk
  | x when x >= 3. && x < 5. -> solid
  | x when x >= 5. && x < 7. -> semi_solid
  | x when x >= 7. && x < 12. -> semi_distant
  | x when x >= 12. && x < 100. -> distant
  | _ -> empty

let cast_cam ~(camera: Camera.t) ~duration figure =
  let open Point3 in
  let horiz_step = camera.width /. (Float.of_int camera.resolution.x) <*> Camera.right camera  in
  let vert_step = -. camera.height /. (Float.of_int camera.resolution.y) <*> Camera.up in

  let init =
    let x = -. camera.width /. 2. <*> Camera.right camera in
    let y = camera.height /. 2. <*> Camera.up in
    let char_off_x = 0.5 <*> horiz_step in
    let char_off_y = 0.5 <*> vert_step in
    camera.center <+> x <+> y <+> char_off_x <+> char_off_y
  in

  Stdio.printf "init:%s  h_step:%s  v_step:%s\n" (Point3.show init) (Point3.show horiz_step) (Point3.show vert_step);

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
      let direction = unit (sample_point <+> neg camera.focus) in
      (* let direction = camera.normal in *)
      let ray = Ray.{ origin = sample_point; direction } in
      let dist = figure.intersect ray in
      (* if y mod 3 = 0 && x mod 3 = 0 || dist != Float.infinity then
       *   Stdio.printf "Origin:%s  Dir:%s  Dist:%f\n" (Point3.show ray.origin) (Point3.show ray.direction) dist; *)
      loop (x + 1) y (acc ^ char_from_distance dist)
    end
  in
  Frame.{contents=loop 0 0 ""; duration }
