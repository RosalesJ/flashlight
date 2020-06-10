type t

val make : ?x:float -> ?y:float -> ?z:float -> unit -> t

val map : t -> f:(float -> float) -> t

val pointwise : t -> t -> f:(float -> float -> float) -> t

val add : t -> t -> t

val l2 : t -> float

val dist : t -> t -> float

val scale : t -> c:float -> t

val neg : t -> t

val show : t -> string

val (<+>) : t -> t -> t

val dot : t -> t -> float

val cross : t -> t -> t

val (<*>) : float -> t -> t

val approx : t -> t -> bool

val unit : t -> t

val origin : t

val tuple : t -> float * float * float

val x : t -> float
val y : t -> float
val z : t -> float
