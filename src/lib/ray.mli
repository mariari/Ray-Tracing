open Vector

type t = {
    origin : Vec3.t;
    dir    : Vec3.t
}

val at : t -> float -> Vec3.t
