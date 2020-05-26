(* open Lib *)
open Core

type cord = {
    width  : int;
    height : int
  }

let generate_header {width; height} =
  String.concat
    [ "P3\n"
    ; Int.to_string width
    ; " "
    ; Int.to_string height
    ; "\n255\n" ]


(* (i / 255) * 255.999  =  i * 255.999 / 255*)

(* Calsl the applicaitve, generating all combinations *)
let combinations xs ys =
   let module M = Applicative.Of_monad(Sequence) in
   M.apply (M.map ~f:(fun x y -> x, y) xs) ys

(* generates the ranges for a given height and width *)
let generate_values {height; width} =
  let seq ?(stride=1) x y =
    Sequence.range ~stride ~start:`inclusive ~stop:`inclusive x y
  in
  let heights = seq ~stride:(-1) (Int.pred height) 0 in
  let widths  = seq 0 (Int.pred width) in
  combinations heights widths

(* Book does this formula for some reason *)
(* Seems like a normalization step *)
(* (i / 255) * 255.999  =  i * 255.999 / 255*)
let generate_values_normalize ({height; width} as cord) =
  generate_values cord
  |> Sequence.map
      ~f:(fun (j, i) ->
        let trans curr orig =
          Float.of_int curr /. Float.of_int (Int.pred orig) *. 255.999
          |> Int.of_float
         in
         let r = trans i width in
         let g = trans j height in
         let b = Int.of_float (255.999 *. 0.25) in
         r, g, b)

let () =
  let cords = {width = 256; height = 256} in
  Out_channel.output_string stdout (generate_header cords);
  generate_values_normalize cords
  |> Sequence.iter
      ~f:(fun (r,g,b) ->
        if Int.(b = 63)
        then Out_channel.output_string
               stderr
               String.("\r Scanlines remaining: " ^ Int.to_string g ^ " ");
             Out_channel.flush stderr;
        String.(concat ~sep:" "
                      (List.map ~f:Int.to_string [r; g; b])
                ^ "\n")
        |> Out_channel.output_string stdout
      )
