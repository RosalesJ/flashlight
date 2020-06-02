open Point3

type resolution = { x: int; y: int }

type t = { center : Point3.t
         ; normal : Point3.t
         ; focus : Point3.t
         ; height : float
         ; width : float
         ; resolution: resolution}

let char_height = 28.
let char_width = 15.

let terminal_aspect = char_height /. char_width

let aspect camera = camera.height /. camera.width

let show {center; normal; height; width; resolution; focus } =
  Printf.sprintf "center: %s normal: %s (%.1fx%0.1f) ((%dx%d)) %s"
    (Point3.show center) (Point3.show normal) height width resolution.x resolution.y (Point3.show focus)

let from_terminal ?(center = Point3.make ~z:(-1.) ()) ?(zoom = 1.) term_width term_height =
  let height = ((Float.of_int term_height) *. char_height) *. zoom in
  let width = ((Float.of_int term_width) *. char_width) *. zoom in
  let normal = Point3.make ~z:(1.) () in
  let focus = -. width /. 2. <*>  normal in
  { height; width; center; normal; resolution={x=term_width; y=term_height}; focus }

let up = Point3.make ~y:1. ()

let right camera = Point3.cross up camera.normal

let project camera point =
  let v_perp = scale ~c:(dot camera.normal point) camera.normal in
  point <+> (neg v_perp)

let right_dist camera point =
  let right = right camera in
  let right_component = dot right point <*> right in
  l2 right_component

let down_dist point =
  let down = neg up in
  let down_component = dot down point <*> down in
  l2 down_component
