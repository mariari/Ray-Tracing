type t = {
    x : float;
    y : float;
    z : float
}

include Signature.Vector3 with type t := t

include Utilitie.Sig with type t := t
