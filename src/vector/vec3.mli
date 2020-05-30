type foo = {
    x : float;
    y : float;
    z : float
}

type t = foo

include Signatures.Vector3 with type t := t

include Utilites.Sig with type t := t
