module type Sig = sig
  type t

  val to_string : t -> string
  val sub       : t -> t -> t
  val prod_vec  : t -> t -> t
  val div_vec   : t -> t -> t
  val dot    : t -> t -> float
  val cross  : t -> t -> t
  val unit   : t -> t
  val ( +| ) : t -> t -> t
  val ( *| ) : float -> t -> t
  val ( -| ) : t -> t -> t
end

module T : functor (Vec : Signature.Vector3) ->
  sig
    include Sig with type t = Vec.t
end
