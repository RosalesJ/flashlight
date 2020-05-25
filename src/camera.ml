type t =
  { x_min: float
  ; x_max: float
  ; y_min: float
  ; y_max: float
  }

let char_height = 28.
let char_width = 15.

let terminal_aspect = char_height /. char_width

let height {x_min; x_max; _} =
  x_max -. x_min

let width {y_min; y_max; _} =
  y_max -. y_min

let aspect camera =
  width camera /. height camera

let show {x_min; x_max; y_min; y_max} =
    Printf.sprintf "Camera { x:(%f..%f), y:(%f..%f) }" x_min x_max y_min y_max

let from_terminal width height ?center:(center = 0., 0.) ~zoom:zoom =
  let xc, yc = center in
  let d_height = ((Float.of_int height) *. char_height /. 2.) *. zoom in
  let d_width = ((Float.of_int width) *. char_width /. 2.) *. zoom in

  { x_min = xc -. d_height
  ; x_max = xc +. d_height
  ; y_min = yc -. d_width
  ; y_max = yc -. d_width
  }
