let cast_cam (cam : Camera.t) x_samples y_samples ~f =
  let rec loop p acc =
    if Camera.down_dist p >= cam.height /. 2. then
      acc
    else if Camera.right_dist cam p >= cam.width /. 2. then
      loop cam.x_min (y +. y_step) (acc ^ "\n")
    else begin
      let rendered_value = f Point3.{x; y; z=0.} in
      loop (x +. x_step) y (acc ^ rendered_value)
    end
  in
  loop cam.x_min cam.y_min ""
