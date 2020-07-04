open Base

module type Figure_instance = sig
  include Figure.T
  val this : t
end

type t = (module Figure_instance) list

let build_instance (type a) (module X : Figure.T with type t = a) x =
  (module struct include X let this = x end : Figure_instance)

let of_figures fig_mod =
  let instance_mod = build_instance fig_mod in
  List.map ~f:instance_mod

let union s1 s2 = List.concat [s1; s2]

let singleton mod_fig s =
  [build_instance mod_fig s]

let empty = []

let insert mod_fig x scene =
  build_instance mod_fig x :: scene

let intersect_all figures ray =
  let dist =
  figures
  |> List.map ~f:(fun (module X : Figure_instance) -> (X.intersect ray X.this).dist)
  |> List.fold ~f:Float.min ~init:Float.infinity
  in Figure.{dist}
