(* open Lib *)
open Core

open Vector

type cord = {
    width  : int;
    height : int
  }

let generate_header {width; height} =
  String.concat
    [ "P3\n" ; Int.to_string width ; " " ; Int.to_string height ; "\n255\n" ]


(* (i / 255) * 255.999  =  i * 255.999 / 255*)

(* Calsl the applicaitve, generating all combinations *)
let combinations xs ys =
   let module M = Applicative.Of_monad(Sequence) in
   M.apply (M.map ~f:(fun x y -> x, y) xs) ys

(* generates the ranges for a given height and width *)
let generate_values {height; width} =
  let heights = Sequence.range ~stride:(-1) (Int.pred height) (-1) in
  let widths  = Sequence.range 0 width in
  combinations heights widths

(* Book does this formula for some reason *)
(* Seems like a normalization step *)
(* (i / 255) * 255.999  =  i * 255.999 / 255*)
let generate_values_normalize ({height; width} as cord) =
  generate_values cord
  |> Sequence.map
      ~f:(fun (j, i) ->
        let trans curr orig =
          Float.of_int curr /. Float.of_int (Int.pred orig)
        in
         Color.create_t ~r:(trans i width) ~g:(trans j height) ~b:0.25)


let report_progress_on_zero r g =
  if Float.(r = 0.)
  then Out_channel.output_string
         stderr
         ("\r Scanlines remaining: " ^ Int.to_string (Int.of_float g) ^ " ");
  Out_channel.flush stderr

let () =
  let cords = {width = 256; height = 256} in
  Out_channel.output_string stdout (generate_header cords);
  generate_values_normalize cords
  |> Sequence.iter
      ~f:(fun col ->
        let normal = Color.normalize col in
        report_progress_on_zero (Color.r normal) (Color.g normal);
        Color.write_no_normal normal)
