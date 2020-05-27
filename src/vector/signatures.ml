module type Vector3 = sig
  type t

  val create : x:float -> y:float -> z:float -> t

  val x : t -> float

  val y : t -> float

  val z : t -> float

  val neg : t -> t

  val pointwise : f:(float -> float -> float) -> t -> t -> t

  val map : f:(float -> float) -> t -> t

  val add : t -> t -> t

  val prod : float -> t -> t

  val div : float -> t -> t

  val squared : t -> float

  val l2_norm : t -> float

  val length : t -> float

  val to_cordinates : t -> Types.cordinates
end
