type t

val empty : t

val of_figures : (module Figure.Movable with type t = 'a) -> 'a list -> t
val of_non_movable : (module Figure.T with type t = 'a) -> 'a list -> t

val union : t -> t -> t

val insert_movable : (module Figure.Movable with type t = 'a) -> 'a -> t -> t
val insert_non_movable : (module Figure.T with type t = 'a) -> 'a -> t -> t

val intersect_all : t -> Ray.t -> float
