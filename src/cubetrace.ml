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

let _triangle_test=
  let triangle = Figure.Triangle.{
      b = Point3.make ~xyz:(-6., 2., 1.);
      c = Point3.make ~xyz:(4., -3., 5.);
      a = Point3.make ~xyz:(11., 7., 10.)
    }
  in
  Scene.insert (module Figure.Triangle) triangle Scene.empty

let _square_test =
  let triangle = Figure.Square.{
      a = Point3.make ~xyz:(-1., 1., 1.);
      c = Point3.make ~xyz:(1., 1., 1.);
      b = Point3.make ~xyz:(-1., -1., 1.);
      d = Point3.make ~xyz:(1., -1., 1.)
    }
  in
  Scene.insert (module Figure.Square) triangle Scene.empty

let _cube_test =
  let center = Point3.make ~xyz:(-3.5, -4.5, 5.) in
  let r = 3. in
  let cube = Figure.make_cube center r in

  Scene.insert (module Figure.Cube) cube Scene.empty

let _final_test camera =
  [Figure.Sphere.{ center = (Point3.make ~xyz:(0., 0., 10.)); radius = 5. }]
  |> Scene.of_figures (module Figure.Sphere)
  |> Render.cast_cam ~camera ~duration:3.
  |> ignore


let () =
  let camera = init () in
  let duration = 5. in
  (* _plain_test camera; *)
  (* _render_test camera; *)
  (* _cam_test camera; *)
  (* _final_test camera; *)
  _cam_test camera;
  (* _triangle_test *)
  (* |> cast_cam ~camera ~duration *)
  (* |> ignore; *)
  _cube_test
  |> render_scene camera duration;
  ignore ()

