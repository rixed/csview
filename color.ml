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

let to_string = to_html
let of_string s =
  let from_char c0 c =
    Char.code c - Char.code c0 in
  let of_char c =
    if c >= '0' && c <= '9' then from_char '0' c
    else if c >= 'a' && c <= 'f' then from_char 'a' c
    else if c >= 'A' && c <= 'F' then from_char 'A' c
    else invalid_arg s in
  let get i =
    let hi = of_char s.[i]
    and lo = of_char s.[i+1] in
    float_of_int (hi*16 + lo) /. 256. in
  if String.length s = 7 && s.[0] = '#' then
    [| get 1 ; get 3 ; get 5 |]
  else
    invalid_arg s (* TODO: other format *)

let print fmt c = Printf.fprintf fmt "%s" (to_string c)

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

