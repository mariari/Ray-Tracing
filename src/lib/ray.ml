open Vector

type t = {
    origin : Vec3.t;
    dir    : Vec3.t
}

let at {origin; dir} pos =
  Vec3.(pos *| dir +| origin)
