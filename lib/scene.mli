type t

val empty : t

val singleton : (module Figure.T with type t = 'a) -> 'a -> t

val of_figures : (module Figure.T with type t = 'a) -> 'a list -> t

val union : t -> t -> t

val insert : (module Figure.T with type t = 'a) -> 'a -> t -> t

val intersect_all : t -> Ray.t -> float
