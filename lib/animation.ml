type t = Render.Frame.t list


let linear duration trans framerate camera cube =
  let total_frames = Float.to_int (framerate /. duration) in
  
  let rec loop i state acc =
    if i > total_frames then
      acc
    else begin
      let scene = Scene.of_figures (module Figure.Cube) [state] in
      let frame = Render.cast_cam ~camera ~duration:framerate scene in
      
      let new_state = Figure.Cube.move trans state in
      loop (i + 1) new_state (frame :: acc)
    end
  in
  loop 0 cube []
        
