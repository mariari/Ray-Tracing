
module M : (Signatures.Vector3 with type t = Vec3.t) = Vec3

include  M

include Utilites.T(M)

let normalize_color Vec3.{x; y=_; z=_} = x
