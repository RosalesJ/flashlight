open Render

type t = Frame.t list


let linear duration framerate camera cube trans =
  let total_frames = Float.to_int (framerate *. duration) in
  
  let rec loop i state acc =
    if i > total_frames then
      acc
    else begin
      let scene = Scene.of_figures (module Figure.Cube) [state] in
      let frame = Frame.with_duration ~duration:(1. /. framerate) @@ Render.cast_cam ~camera scene in
      
      let new_state = Figure.Cube.move trans state in
      loop (i + 1) new_state (frame :: acc)
    end
  in
  loop 0 cube []
  |> List.rev


let linear_tri duration framerate camera triangle trans =
  let total_frames = Float.to_int (framerate *. duration) in
  
  let rec loop i state acc =
    if i > total_frames then
      acc
    else begin
      let scene = Scene.of_figures (module Figure.Triangle) [state] in
      let frame = Frame.with_duration ~duration:(1. /. framerate) @@ Render.cast_cam ~camera scene in
      
      let new_state = Figure.Triangle.move trans state in
      loop (i + 1) new_state (frame :: acc)
    end
  in
  loop 0 triangle []
  |> List.rev
        
