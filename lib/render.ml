open ANSITerminal
open Base

let clear_canvas () =
  erase Screen;
  set_cursor 1 1

let start_canvas () =
  ignore (Unix.system "tput smcup")

let end_canvas () =
  ignore (Unix.system "tput rmcup")

let blk = "▓"
let solid = "*"
let semi_solid = "o"
let semi_distant = "."
let distant = "`"
let empty = " "
let negative = "?"

let char_from_distance =
  let open Float in
  function
  | x when x < 0. -> negative
  | x when x >= 0. && x < 3. -> blk
  | x when x >= 3. && x < 5. -> solid
  | x when x >= 5. && x < 7. -> semi_solid
  | x when x >= 7. && x < 12. -> semi_distant
  | x when x >= 12. && x < 100. -> distant
  | _ -> empty

module Frame = struct
  type t = { contents: string; duration: float}

  let render {contents; duration} =
    clear_canvas ();
    (* This allows it to render properly *)
    let contents = String.sub contents ~pos:0 ~len:((String.length contents) - 1) in
    Stdio.printf "%s" contents;
    Stdlib.flush Stdio.stdout;
    Unix.sleepf duration

  let make ~duration snapshot =
    let contents = snapshot
    |> List.map ~f:(List.map ~f:char_from_distance)
    |> List.map ~f:(String.concat)
    |> String.concat ~sep:"\n"
    in {duration; contents}
end
