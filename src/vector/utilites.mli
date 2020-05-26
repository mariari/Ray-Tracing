module T : functor (Vec : Signatures.Vector3) ->
  sig
    type t = Vec.t
    val to_string : t -> string
    val sub : t -> t -> t
    val prod_vec : t -> t -> t
    val div_vec : t -> t -> t
    val dot : t -> t -> float
    val cross : t -> t -> t
    val unit : t -> t
end


module type Sig = sig
  type t
  val to_string : t -> string
  val sub : t -> t -> t
  val prod_vec : t -> t -> t
  val div_vec : t -> t -> t
  val dot : t -> t -> float
  val cross : t -> t -> t
  val unit : t -> t
end
