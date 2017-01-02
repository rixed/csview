open Batteries

let make_time_serie t1 t2 dt dt_spread =
  assert (dt_spread < dt) ;
  let rec loop ts t =
    if t > t2 then List.rev ts |> Array.of_list else
    let t' = t +. dt +. (Random.float dt_spread -. dt_spread *. 0.5) in
    loop (t::ts) t' in
  loop [] t1

let hourly_cycles ts =
  Array.map (fun t ->
    2. +.
    0.2 *. sin (0.13 +. t *. 6.2832) +. (* fast 1s bumps *)
    sin (t *. 0.0017453333)) ts

let find_next_closer t ts i =
  let len = Array.length ts in
  assert (i < len) ;
  (* TODO *)
  let rec loop i =
    if i >= Array.length ts then i-1 else
    if ts.(i) = t then i else
    if ts.(i) > t then (
      if i = 0 then i else
      let d_before = t -. ts.(i-1)
      and d_after = ts.(i) -. t in
      if d_before <= d_after then i-1 else i
    ) else
    loop (i+1) in
  loop i
(*$= find_next_closer & ~printer:string_of_int
  1 (find_next_closer   1.0 [| 0.; 1.; 2. |] 0)
  1 (find_next_closer   1.1 [| 0.; 1.; 2. |] 0)
  1 (find_next_closer   0.9 [| 0.; 1.; 2. |] 0)
  0 (find_next_closer   0.0 [| 0.; 1.; 2. |] 0)
  0 (find_next_closer   0.1 [| 0.; 1.; 2. |] 0)
  0 (find_next_closer (-0.1)[| 0.; 1.; 2. |] 0)
  2 (find_next_closer   42. [| 0.; 1.; 2. |] 0)
 *)

(* We must return not more than n indices but we can return less if we do have
 * less data (avoid sending several times the same index!). We also want to have
 * indices evenly spaced in time (not in the index space!) therefore we could also
 * return less than n indices if we have more than n times in between t1 and t2 if
 * they are clustered. *)
let select n t1 t2 ts =
  if n = 0 || t1 > t2 then [||] else
  if n = 1 then let tmid = (t2 -. t1) *. 0.5 in
    [| find_next_closer tmid ts 0 |] else
  (* Impossible to use fancy algorithm since times in ts are not necessarily
   * evenly spaced :( *)
  let dt = (t2 -. t1) /. float_of_int (n-1) in
  let rec loop prev t i last_i =
    if t > t2 then Array.of_list (List.rev prev) else
    let prev',last_i' =
      if i != last_i then i :: prev, i
      else prev, last_i
    and t' = t +. dt in
    let i' = find_next_closer t' ts i in
    loop prev' t' i' last_i' in
  loop [] t1 0 ~-1

let project is a =
  Array.map (fun i -> a.(i)) is

type graph = {
  ts : float array ;
  ds : float array list
}

let graph =
  let ts1 =
    let now = Unix.gettimeofday () in
    let t1 = now -. Random.float (3600. *. 10.) in
    make_time_serie t1 now 60. 0.15 in
  { ts = ts1 ; ds = [ hourly_cycles ts1 ] }

let get_graph file t1 t2 n =
  Printf.eprintf "Getting the graph for %s between %f and %f, %d points\n%!"
    file t1 t2 n ;
  let is = select n t1 t2 graph.ts in
  Printf.eprintf "indices selected\n%!" ;
  { ts = project is graph.ts ;
    ds = List.map (project is) graph.ds }

let oldest _file = graph.ts.(0)
let latest _file = graph.ts.(Array.length graph.ts - 1)
