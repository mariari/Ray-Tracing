open Core_kernel

module M : (Signature.Vector3 with type t = Vec3.t) = Vec3

include  M

(* Past OCaml 4.08 you can just say *)
(* include Utilitie.T(M) *)
include (Utilitie.T(M) : module type of Utilitie.T(M) with type t := t)

let normalize = prod 255.999

let normalize_int col =
  let Vec3.{x;y;z} = normalize col in
  Int.of_float x, Int.of_float y, Int.of_float z

let create_t ~r ~g ~b = create ~x:r ~y:g ~z:b

let r = x

let g = y

let b = z

(** [write_no_normal] prints the color as is, without any normalization  *)
let write_no_normal Vec3.{x;y;z} ~file =
  String.(concat ~sep:" "
                 (List.map ~f:(Fn.compose Int.to_string Float.to_int) [x; y; z]) ^ "\n")
  |> Out_channel.output_string file

(** [write] normalizes the color then prints it *)
let write col ~file =
  normalize col
  |> write_no_normal ~file
