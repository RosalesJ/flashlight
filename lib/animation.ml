open Render
open Base

type t = Frame.t list

let linear duration framerate camera cube trans =
  let total_frames = Float.to_int (framerate *. duration) in
  
  let rec loop i state acc =
    if i > total_frames then
      acc
    else begin
      let scene = Scene.of_figures (module Figure.Cube) [state] in
      let frame = Camera.capture ~camera scene in
      
      let new_state = Figure.Cube.move trans state in
      loop (i + 1) new_state (frame :: acc)
    end
  in
  loop 0 cube []
  |> List.rev_map ~f:(Frame.make ~duration:(1. /. framerate))


let linear_tri duration framerate camera triangle trans =
  let total_frames = Float.to_int (framerate *. duration) in
  
  let rec loop i state acc =
    if i > total_frames then
      acc
    else begin
      let scene = Scene.of_figures (module Figure.Triangle) [state] in
      let frame = Camera.capture ~camera scene in
      
      let new_state = Figure.Triangle.move trans state in
      loop (i + 1) new_state (frame :: acc)
    end
  in
  loop 0 triangle []
  |> List.rev_map ~f:(Frame.make ~duration:(1. /. framerate))
        
