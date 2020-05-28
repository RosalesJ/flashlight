open Point3

type t = { center : Point3.t
         ; normal : Point3.t
         ; height : float
         ; width : float }

let char_height = 28.
let char_width = 15.

let terminal_aspect = char_height /. char_width

let aspect camera = camera.height /. camera.width

let show {center; normal; height; width} =
    Printf.sprintf "center: %s normal: %s height: %f3 weight: %f3" (Point3.show center) (Point3.show normal) height width

let from_terminal ?(center = Point3.make ~z:1. ()) ?(zoom = 1.) width height =
  let height = ((Float.of_int height) *. char_height) *. zoom in
  let width = ((Float.of_int width) *. char_width) *. zoom in
  let normal = Point3.make ~z:(-1.) () in
  { height; width; center; normal }

let up = Point3.make ~y:1. ()

let right camera = Point3.cross up camera.normal

let project camera point =
  let v_perp = scale ~c:(dot camera.normal point) camera.normal in
  point + (neg v_perp)

let right_dist camera point =
  let right = right camera in
  let right_component = Point3.scale right ~c:(Point3.dot right point) in
  Point3.l2 right_component

let down_dist point =
  let down = Point3.neg up in
  let down_component = Point3.scale down ~c:(Point3.dot down point) in
  Point3.l2 down_component
