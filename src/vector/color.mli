include Signatures.Vector3

include Utilites.Sig with type t := t

val create_t : r:float -> g:float -> b:float -> t

val normalize : t -> t
val normalize_int : t -> int * int * int

val r : t -> float
val g : t -> float
val b : t -> float

val write : t -> unit
val write_no_normal : t -> unit
