module type Figure_instance = sig
  include Figure.T
  val this : t
end

module type Movable_instance = sig
  include Figure.Movable
  val this : t
end

type is_movable =
  | Movable of (module Movable_instance)
  | Non_movable of (module Figure_instance)

type t = is_movable list

let build_movable (type a) (module X : Figure.Movable with type t = a) x =
  Movable (module struct include X let this = x end : Movable_instance)

let build_non_movable (type a) (module X : Figure.T with type t = a) x =
  Non_movable (module struct include X let this = x end : Figure_instance)

let empty = []

let of_movable fig_mod =
  let instance_mod = build_movable fig_mod in
  List.map instance_mod

let of_non_movable fig_mod =
  let instance_mod = build_non_movable fig_mod in
  List.map instance_mod

let union s1 s2 = List.concat [s1; s2]

let insert f mod_fig x scene =
  f mod_fig x :: scene

let insert_movable = insert build_movable

let insert_non_movable = insert build_non_movable

let intersect_all (figures : t) ray =
  let f = function
    | Movable (module X)     -> X.intersect ray X.this
    | Non_movable (module X) -> X.intersect ray X.this
  in figures
  |> List.map f
  |> List.fold_left Float.min Float.infinity

