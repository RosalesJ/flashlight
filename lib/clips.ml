open Base

let triangle_spin camera =
  let framerate = 10. in
  let duration = 10. in
  
  let triangle = Figure.Triangle.{
      b = Point3.make ~xyz:(-6., 2., 1.);
      c = Point3.make ~xyz:(4., -3., 5.);
      a = Point3.make ~xyz:(6.,  7., 6.)
    }
  in
  let rot_x = Transforms.rotation_x 0.2 in
  let rot_y = Transforms.rotation_y 0.3 in

  Affine.compose rot_x rot_y
  |> Transforms.transform_about (Figure.triangle_center triangle)
  |> Animation.linear_tri duration framerate camera triangle


let cube_spin camera =
  let center = Point3.make ~xyz:(0., 0., 5.5) in
  let r = 3. in
  let cube = Figure.make_cube center r in

  let framerate = 10. in
  let duration = 6. in

  let rot_x = Transforms.rotation_x 0.3 in
  let rot_y = Transforms.rotation_y 0.1 in

  Affine.compose rot_x rot_y
  |> Transforms.transform_about center
  |> Animation.linear duration framerate camera cube
