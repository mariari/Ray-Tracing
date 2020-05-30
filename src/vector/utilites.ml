open Base

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
end

module T (Vec : Signatures.Vector3) = struct
  type t = Vec.t

  let to_string vec =
    let Types.{x; y; z} = Vec.to_cordinates vec in
    String.concat ~sep:" " (List.map ~f:Float.to_string [x; y; z])

  let sub = Vec.pointwise ~f:(-.)

  let prod_vec = Vec.pointwise ~f: ( *. )
  let div_vec  = Vec.pointwise ~f: ( /. )

  let dot vec1 vec2 =
    prod_vec vec1 vec2
    |> Vec.to_cordinates
    |> fun Types.{x; y; z} ->
      x +. y +. z


  let cross vec1 vec2 =
    let cord1 = Vec.to_cordinates vec1 in
    let cord2 = Vec.to_cordinates vec2 in
    Vec.create ~x:(cord1.y *. cord2.z -. cord1.z *. cord2.y)
               ~y:(cord1.z *. cord2.x -. cord1.x *. cord2.z)
               ~z:(cord1.x *. cord2.y -. cord1.y *. cord2.x)

  let unit vec =
    Vec.div 3. vec

  let ( +| ) = Vec.add
  let ( *| ) = Vec.prod
end
