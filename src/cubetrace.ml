open ANSITerminal
open Lib
open Render
open Base

let init () =
  let width, height = size () in
  Camera.from_terminal width height ~zoom:0.004

let _cam_test camera =
  Stdio.printf "%s\n" (Camera.show camera)

let _plane_b =  Figure.Plane.{ origin = (Point3.make ~xyz:(0., -5., 0.));  normal = Camera.up }
let plane_d =   Figure.Plane.{ origin = (Point3.make ~xyz:(-6., -2., 0.)); normal = Point3.unit (Point3.make ~xyz:(1., 1., 0.)) }
let _plane_dz = Figure.Plane.{ origin = (Point3.make ~xyz:(-3., -2., 0.)); normal = Point3.unit (Point3.make ~xyz:(1., 1., -1.)) }

let _sphere =  Figure.Sphere.{ center = (Point3.make ~xyz:(-2., 0., 10.)); radius = 6. }
let _sphere2 = Figure.Sphere.{ center = (Point3.make ~xyz:(6., 0., 4.)); radius = 2. }

let render_scene camera duration scene =
  start_canvas ();
  scene
  |> cast_cam ~camera ~duration
  |> Frame.render;
  end_canvas ()

let draw_triangle t camera duration =
  start_canvas ();
  Scene.singleton (module Figure.Triangle) t
  |> cast_cam ~camera ~duration
  |> Frame.render;
  end_canvas ()

let render_animation anim =
  start_canvas ();
  List.iter ~f:Frame.render anim;
  end_canvas ()
    
let _translate_spheres =
  let rec loop n acc =
    if n = 0 then
      acc
    else
      loop (n - 1)
        (Figure.Sphere.{ center = (Point3.make ~xyz:(0., 0., 8.5 +. (Float.of_int n) /. 3.)) ; radius = 8. }
         :: acc)
  in
  let scene = loop 30 [] in
  Scene.of_figures (module Figure.Sphere) scene

let _plain_test camera =
  let _plane_v = Figure.Plane.{ origin = (Point3.make ~xyz:(-5., 0., 0.)); normal = (Camera.right camera)} in
  (* [_plane_b; _plane_v; (\* plane_d; plane_dz *\)] *)
  Scene.empty
  |> Scene.insert (module Figure.Plane) _plane_b
  |> Scene.insert (module Figure.Plane) _plane_v
  (* |> Scene.insert (module Figure.Plane) _plane_dz *)
  |> Scene.insert (module Figure.Sphere) _sphere
  |> Scene.insert (module Figure.Sphere) _sphere2

let triangle_center Figure.Triangle.{a; b; c} =
  let (ax, ay, az) = Point3.tuple a in
  let (bx, by, bz) = Point3.tuple b in
  let (cx, cy, cz) = Point3.tuple c in
  Point3.make ~xyz:(
    (ax +. bx +. cx) /. 3.,
    (ay +. by +. cy) /. 3.,
    (az +. bz +. cz) /. 3.)

let _triangle_test camera =
  let triangle = Figure.Triangle.{
      b = Point3.make ~xyz:(-6., 2., 1.);
      c = Point3.make ~xyz:(4., -3., 5.);
      a = Point3.make ~xyz:(11., 7., 10.)
    }
  in
  let center = triangle_center triangle in

  draw_triangle triangle camera 1.;
  
  let rot = Transforms.rotation_y 0.2 in
  let trans = Transforms.translation (Point3.neg center) in
  let trans_back = Transforms.translation center in
  let t = Affine.compose trans_back (Affine.compose rot trans) in
  draw_triangle (Figure.Triangle.move t triangle) camera 3.

let _triangle_animation camera =
  start_canvas ();
  let framerate = 10. in
  let duration = 10. in
  (* let distance = 5. in *)
  (* let velocity = distance /. (framerate *. duration) in *)
  
  let triangle = Figure.Triangle.{
      b = Point3.make ~xyz:(-6., 2., 1.);
      c = Point3.make ~xyz:(4., -3., 5.);
      a = Point3.make ~xyz:(11., 7., 10.)
    }
  in
  let center = triangle_center triangle in
  let rot_x = Transforms.rotation_x 0.2 in
  let rot_y = Transforms.rotation_y 0.2 in
  
  (* let trans = Transforms.translation (Point3.neg center) in
   * let trans_back =p Transforms.translation center in  
   * let t = Affine.compose trans_back (Affine.compose (Affine.compose rot_x rot_y) trans) in *)

  let t = Transforms.transform_about center (Affine.compose rot_x rot_y) in
  
  let anim = Animation.linear_tri duration framerate camera triangle t in
  List.iter ~f:Frame.render anim;
  end_canvas ()

let _square_test =
  let triangle = Figure.Square.{
      a = Point3.make ~xyz:(-1.,  1.,  1.);
      c = Point3.make ~xyz:(1.,   1.,  1.);
      b = Point3.make ~xyz:(-1., -1.,  1.);
      d = Point3.make ~xyz:(1.,  -1.,  1.)
    }
  in
  Scene.insert (module Figure.Square) triangle Scene.empty

let _cube_test =
  let center = Point3.make ~xyz:(-3.5, -4.5, 5.) in
  let r = 3. in
  let cube = Figure.make_cube center r in

  Scene.insert (module Figure.Cube) cube Scene.empty

let _cube_animation cam =
  let center = Point3.make ~xyz:(-3.5, -4.5, 5.) in
  let r = 3. in
  let cube = Figure.make_cube center r in

  let trans =
    Transforms.rotation_x 10.
    |> Transforms.transform_about center
  in

  Animation.linear 5. 5. cam cube trans

let _final_test camera =
  [Figure.Sphere.{ center = (Point3.make ~xyz:(0., 0., 10.)); radius = 5. }]
  |> Scene.of_figures (module Figure.Sphere)
  |> Render.cast_cam ~camera ~duration:3.
  |> ignore

let () =
  let camera = init () in
  (* _plain_test camera; *)
  (* _render_test camera; *)
  (* _cam_test camera; *)
  (* _final_test camera; *)
  _cam_test camera;
  _triangle_animation camera;
  (* |> cast_cam ~camera ~duration *)
  (* |> ignore; *)
  (* _cube_test *)
  (* |> render_scene camera duration; *)
  (* _cube_animation camera
   * |> render_animation; *)
  ignore ()
