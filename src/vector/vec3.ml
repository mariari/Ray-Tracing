open Base

type foo = {
    x : float;
    y : float;
    z : float
}

module Impl : sig
  type nonrec t = foo
  include Signatures.Vector3 with type t := t
end = struct

  type nonrec t = foo

  let create ~x ~y ~z = {x; y; z}

  let neg {x; y; z} =
    { x = Float.neg x;
      y = Float.neg y;
      z = Float.neg z }


  let pointwise ~f vec1 vec2 =
    { x = f vec1.x vec2.x
    ; y = f vec1.y vec2.y
    ; z = f vec1.z vec2.z
    }

  let map ~f {x; y; z} =
    { x = f x
    ; y = f y
    ; z = f z
    }

  let x t = t.x

  let y t = t.y

  let z t = t.z

  let add = pointwise ~f:(+.)

  let prod x = map ~f:(( *. ) x)

  let div x = prod (1. /. x)

  let squared {x; y; z} =
    x *. x +. y *. y +. z *. z

  let l2_norm t =
    squared t
    |> Float.sqrt

  let length = l2_norm

  (* Does this incur a cost *)
  let to_cordinates {x;y;z} = Types.{x;y;z}

  let from_cordinates Types.{x;y;z} = {x;y;z}
end

include Types

include Impl

include Utilites.T(Impl)
