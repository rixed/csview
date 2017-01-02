(* Simple color scales *)
open Batteries

type t = float array
type scale = (float * t) list (* ordered list of value + color *)

let white = [| 1.;1.;1. |]

let to_html c =
  IO.to_string (
    Array.print ~first:"#" ~last:"" ~sep:""
      (fun oc f ->
        Printf.fprintf oc "%02x" (int_of_float (255.*.f)))) c

let get s v =
  let rec aux (start, start_col) = function
    | [] -> white
    | (stop, stop_col)::s' ->
      if v = stop then stop_col else
      if v > stop then aux (stop, stop_col) s' else
      let r = (v -. start) /. (stop -. start) in
      Array.mapi (fun i stop_c ->
        let start_c = if Array.length start_col <= i then 0.
                      else start_col.(i) in
        start_c +. r *. (stop_c -. start_c))
        stop_col in
  aux (0., [||]) s |> to_html

let nb_random_colors = 64
let random_colors =
  let frac v = fst (modf v) in
  Enum.from_loop
    (102, 311, 67)
    (fun (r,g,b) ->
      [| frac (float_of_int r /.255.) ;
         frac (float_of_int g /.255.) ;
         frac (float_of_int b /.255.) |],
      (r + 133, g + 39, b + 247)) |>
  Enum.take nb_random_colors |>
  Array.of_enum

let random_of_string str =
  let i = Hashtbl.hash str in random_colors.(i mod nb_random_colors)

