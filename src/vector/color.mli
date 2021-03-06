include Signature.Vector3

include Utilitie.Sig with type t := t

val create_t : r:float -> g:float -> b:float -> t

val normalize : t -> t
val normalize_int : t -> int * int * int

val r : t -> float
val g : t -> float
val b : t -> float

val write           : t -> file:Core_kernel.Out_channel.t -> unit
val write_no_normal : t -> file:Core_kernel.Out_channel.t -> unit
