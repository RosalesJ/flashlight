type t

val apply : t -> Point3.t -> Point3.t

val compose : t -> t -> t

val id : t

val from_basis : i:Point3.t -> j:Point3.t -> k:Point3.t -> t

val det : t -> float

val inv : t -> t option

val inv_exn : t -> t

val show : t -> string
