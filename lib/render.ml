let cast_cam (camera : Camera.t) x_samples y_samples ~f =
  let open Point3 in
  let horiz_step = camera.width /. (Float.of_int y_samples) <*> Camera.right camera  in
  let vert_step = camera.height /. (Float.of_int x_samples) <*> Camera.up in

  let init =
    let x = (camera.height /. 2.) <*> Camera.up in
    let y = (-. camera.width /. 2.) <*> Camera.right camera in
    x <+> y
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
      let rendered_value = f sample_point in
      loop (x + 1) y (acc ^ rendered_value)
    end
  in
  loop 0 0 ""
