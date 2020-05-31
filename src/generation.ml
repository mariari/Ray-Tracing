(* open Lib *)
open Core_kernel

open Lib
open Vector

module View = struct
  type t = {
      height       : float;
      width        : float;
      focal_length : float;
    }
end

type cord = {
    width  : int;
    height : int
}

(** [ray_color ray] takes a ray and creates a blends blue and white depending
 * on the y coordinate
 *)
(* notice the math
 * (1 - t, 1 - t, 1 - t) + (½t, .7t, t) = (1 - ½t, 1 - 0.3t, 1)
 *)
let ray_color (r : Ray.t) =
  let unit_dir = Vec3.unit r.dir in
  let t        = 0.5 *. (unit_dir.y +. 1.0) in
  Color.( (1.0 -. t) *| create_t ~r:1.0 ~g:1.0 ~b:1.0
         +|    t     *| create_t ~r:0.5 ~g:0.7 ~b:1.0)


(** [generate_header cord] generates a PPM header with the following dimensions *)
let generate_header {width; height} =
  String.concat
    [ "P3\n" ; Int.to_string width ; " " ; Int.to_string height ; "\n255\n" ]


(* (i / 255) * 255.999  =  i * 255.999 / 255*)
(* generates the ranges for a given height and width *)
let generate_image_range {height; width} =
  let heights = Sequence.range ~stride:(-1) (Int.pred height) (-1) in
  let widths  = Sequence.range                     0         width in
  Sequence.cartesian_product heights widths

(** [spot_in curr orig] gets the ratio within the   *)
let spot_in curr orig =
  Float.of_int curr /. Float.of_int (Int.pred orig)

let value_normalize {height; width} =
  Sequence.map
      ~f:(fun (j, i) ->
         Color.create_t ~r:(spot_in i width) ~g:(spot_in j height) ~b:0.25)

let report_progress_on_zero j i =
  if Int.(i = 0)
  then begin
      Out_channel.output_string
        stderr
        ("\r Scanlines remaining: " ^ Int.to_string j ^ " ");
      Out_channel.flush stderr
  end

let report_progress_on_zero_f r g =
  if Float.(r = 0.)
  then begin
      Out_channel.output_string
        stderr
        ("\r Scanlines remaining: " ^ Int.to_string (Int.of_float g) ^ " ");
      Out_channel.flush stderr
  end

(* Book does this formula for some reason *)
(* Seems like a normalization step *)
(* (i / 255) * 255.999  =  i * 255.999 / 255*)
let generate_values_normalize_gadiant cord =
  generate_image_range cord
  |> value_normalize cord


let generate_values_blue_gradiant
      ~file
      {height; width}
      origin
      View.{height = view_h; focal_length; width = view_w} =
  let horizontal = Vec3.create ~x:view_w ~y:0.     ~z:0. in
  let vertical   = Vec3.create ~x:0.     ~y:view_h ~z:0. in
  let focal_vec  = Vec3.create ~x:0.     ~y:0.     ~z:focal_length in
  let lower_left = Vec3.(origin -| div 2. horizontal -| div 2. vertical -| focal_vec) in
  Sequence.iter
    ~f:(fun (j,i) ->
      let u = spot_in i width in
      let v = spot_in j height in
      let r =
        Ray.{origin;
             dir = Vec3.(lower_left +| u *| horizontal +| v *| vertical -| origin)}
      in
      let pixel_color = ray_color r in
      report_progress_on_zero j i;
      Color.write ~file pixel_color)

let first_image =
  let cords = {width = 256; height = 256} in
  Out_channel.with_file
    ~f:(fun file ->
      Out_channel.output_string file (generate_header cords);
      generate_values_normalize_gadiant cords
      |> Sequence.iter
          ~f:(fun col ->
            let normal = Color.normalize col in
            report_progress_on_zero_f (Color.r normal) (Color.g normal);
            Color.write_no_normal ~file normal))

let second_image =
  let aspect_ratio = 16.0 /. 9.0 in
  let cords =
    {width = 384; height = Int.of_float (384. /. aspect_ratio)}
  in
  let view =
    View.{ height = 2.; width = aspect_ratio *. 2.; focal_length = 1.}
  in
  let origin = Vec3.create ~x:0. ~y:0. ~z:0. in
  Out_channel.with_file
    ~f:(fun file ->
      Out_channel.output_string file (generate_header cords);
      generate_image_range cords
      |> generate_values_blue_gradiant ~file cords origin view
    )

let () =
  (* first_image "foo.ppm" *)
  second_image "foo2.ppm"
