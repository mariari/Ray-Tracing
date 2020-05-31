type foo = {
    x : float;
    y : float;
    z : float
}

type t = foo

include Signature.Vector3 with type t := t

include Utilitie.Sig with type t := t
