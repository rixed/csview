open Batteries

let debug = false

let max_line_size_ever = 10240

let file_size fd = Unix.(lseek fd 0 SEEK_END)

(*$= file_size & ~printer:string_of_int
  11020 (file_size test_data_1_csv)
 *)

let read_at fd ofs bs =
  let open Unix in
  let buf = Bytes.create bs in
  let ofs' = lseek fd ofs SEEK_SET in
  assert (ofs' = ofs) ;
  let r = read fd buf 0 bs in
  assert (r = bs) ;
  Bytes.to_string buf

(* Returns the position of the beginning of the nth field,
 * or Raises Not_found. *)
let rec find_nth_field_from str sep start n =
  assert (n >= 0) ;
  if n = 0 then start else (
    let start' = String.index_from str start sep in
    find_nth_field_from str sep (start'+1) (n-1)
  )

(* Look for next end of field, aka sep or newline *)
let rec sep_index_from str sep start =
  if start >= String.length str then raise Not_found ;
  let c = str.[start] in
  if c = sep || c = '\n' then start
  else sep_index_from str sep (start+1)

(* Extract the field starting at the given position.
 * Raises Not_found if it's not entirely in str. *)
let extract_field str sep line_start fn fos =
  let field_start =
    find_nth_field_from str sep line_start fn in
  let field_stop = sep_index_from str sep field_start in
  String.sub str field_start (field_stop - field_start) |> fos,
  field_stop

(* Returns the fn field of the first line of that block. *)
let get_block_begin str fn sep fos =
  (* Look for the first line start *)
  let line_start = String.index str '\n' + 1 in
  extract_field str sep line_start fn fos |> fst

(* Same as above but look for all values in that block ; returns the offset of
 * the line that's the closest to the value we are looking for. It's OK if
 * that line is not complete but we must consider only the lines which time
 * _is_ fully present. *)
let find_line_in_block str fn sep fos t =
  (* Note: this does not work for the first line of the file... *)
  let line_start = String.index str '\n' + 1 in
  let rec loop (prev_t, _prev_ofs as prev) ofs =
    match extract_field str sep ofs fn fos with
    | exception Not_found ->
      (* So the best we have is the previous one.
       * Note: It is guaranteed that we have had a full line already *)
      prev
    | t', field_stop ->
      let prev' = t', ofs in
      if t' < t then (
        match String.index_from str field_stop '\n' with
        | exception Not_found ->
          (* So that was it *)
          prev'
        | end_of_line ->
          loop prev' (end_of_line+1)
      ) else (
        (* We passed the target t *)
        if ofs = line_start then (
          t', line_start
        ) else (
          let d_before = t -. prev_t (* Will never happen on first iteration of loop because of starting value *)
          and d_after = t' -. t in
          if d_before <= d_after then prev else prev'
        )
      ) in
  loop ((* won't be used I promise *) 0., 0) line_start

(* Find the line with the target timestamp.
 * ofs_start is such that this beginning of a line is <= target and
 * ofs_end may be anywhere in a line that's > target.
 * Since we look only at the first t of each block, the returned offset
 * is close to the target but may be after or even before (if the best
 * approx was the first TS of the next block). *)
let rec find_block fd fn sep fos t bs ofs_start t_start ofs_stop t_stop =
  let delta_ofs = ofs_stop - ofs_start in
  if delta_ofs <= bs then ofs_start else (
    let delta_t = t_stop -. t_start in
    let ofs_mid = ofs_start + int_of_float ((t -. t_start) *.
                  float_of_int delta_ofs /. delta_t) in
    let ofs_mid = max (ofs_start + bs) ofs_mid in
    let ofs_mid = min (ofs_stop - bs) ofs_mid in
    assert (ofs_mid >= ofs_start) ;
    assert (ofs_mid < ofs_stop) ;
    let block_size = min bs (ofs_stop - ofs_mid) in
    assert (ofs_mid + block_size <= ofs_stop) ;
    let str = read_at fd ofs_mid block_size in
    let t_mid = get_block_begin str fn sep fos in
    if t < t_mid then (
      if debug then Printf.eprintf "t <, %d..%d\n" ofs_start ofs_mid ;
      find_block fd fn sep fos t bs ofs_start t_start ofs_mid t_mid
    ) else if t > t_mid then (
      if debug then Printf.eprintf "t >, %d..%d\n" ofs_mid ofs_stop ;
      find_block fd fn sep fos t bs ofs_mid t_mid ofs_stop t_stop
    ) else ofs_mid (* eq *))

let find_line fd fn sep fos t bs ds sz t_first t_last =
  let approx_ofs = find_block fd fn sep fos t bs ds t_first sz t_last in
  (* What we are looking for may be in the previous or next block, so let's
   * read 3 blocks: *)
  let start_ofs = max 0 (approx_ofs - bs)
  and stop_ofs = min (approx_ofs + 2*bs) sz in
  let block_size = stop_ofs - start_ofs in
  let str = read_at fd start_ofs block_size in
  let t_line, line_start = find_line_in_block str fn sep fos t in
  try (
    let line_stop = String.index_from str line_start '\n' in
    (* we want the end of line char to be present as a delimiter *)
    start_ofs + line_start,
    (t_line, String.sub str line_start (line_stop - line_start + 1))
  ) with Not_found ->
    Printf.eprintf "Cannot find line, fn=%d, t=%f, bs=%d, sz=%d,\
                    looking for newline from pos %d\n\
                    We just read from %d to %d, the close block was at %d\n"
      fn t bs sz line_start start_ofs stop_ofs approx_ofs ;
    raise Not_found


(*TARATATA$= find_line & ~printer:identity
   ((* a value buried in the middle: *) \
    "1481411788.3,you found me!") \
     (find_line (file_reader "test_data/example.csv") 0 ',' float_of_string \
      1481411788.3 4096 155077467 |> snd)
   ((* before the first value: *) \
    "1481406597.335100,availabilities-lookup.poum.10/24.0.v0.dev,start-time,1.48141e+09") \
     (find_line (file_reader "test_data/example.csv") 0 ',' float_of_string \
      1481406597. 4096 155077467 |> snd)
   ((* after the last: *) \
    "1481471701.675750,rooms-update-sg.poum.0/1.3.v0.dev,start-time,1.48141e+09") \
     (find_line (file_reader "test_data/example.csv") 0 ',' float_of_string \
      1481471800. 4096 155077467 |> snd)
 *)

let rec get_last_x fd sz bs sep fn fos =
  let bs' = min sz bs in
  let str = read_at fd (sz - bs') bs' in
  (* We discard incomplete last line if any *)
  match String.rindex str '\n' with
  | exception Not_found when bs' = bs ->
    if debug then Printf.eprintf "can't find nl\n" ;
    (* try with bigger block size *)
    assert (bs < max_line_size_ever) ;
    get_last_x fd sz (bs * 2) sep fn fos
  | eol ->
    if debug then Printf.eprintf "found eol at %d\n" eol ;
    (* we must found the beginning of that line *)
    (match String.rindex_from str (eol-1) '\n' with
    | exception (Invalid_argument _ (* eol-1 was -1 *) | Not_found) ->
      if bs' < sz then (
        if debug then Printf.eprintf "can't find sol\n" ;
        assert (bs < max_line_size_ever) ;
        get_last_x fd sz (bs * 2) sep fn fos
      ) else (
        (* we are at the beginning of the file *)
        if debug then Printf.eprintf "we are at beginning of file\n" ;
        extract_field str sep 0 fn fos |> fst
      )
    | last_eol ->
      let sol = last_eol + 1 in
      if debug then Printf.eprintf "found sol at %d\n" sol ;
      extract_field str sep sol fn fos |> fst)

(*$= get_last_x & ~printer:string_of_float
   1483380648. (get_last_x test_data_1_csv 11020 4096 ',' 0 float_of_string)
 *)

let rec get_first_x fd ds sz bs sep fn fos =
  let bs' = min sz bs in
  let str = read_at fd ds bs' in
  try extract_field str sep 0 fn fos |> fst
  with Not_found ->
    assert (bs < max_line_size_ever) ;
    get_first_x fd ds sz (bs * 2) sep fn fos

(*$= get_first_x & ~printer:string_of_float
   1483347348. (get_first_x test_data_1_csv 0 11020 4096 ',' 0 float_of_string)
 *)

(* We must return no more than n indices but we can return less if we do have
 * enough data (avoid sending several times the same index!). We also want to
 * have indices evenly spaced in time (not in the index space!) therefore we
 * could also return less than n indices if we have more than n times in
 * between t1 and t2 but they are clustered. *)
let read_all fd fn sep fos bs ds sz (t_first:float) t_last n t1 t2 =
  if n = 0 || t1 > t2 then [||] else
  if n = 1 then let tmid = (t2 -. t1) *. 0.5 in
    [| find_line fd fn sep fos tmid bs ds sz t_first t_last |> snd |] else
  (* Impossible to use fancy algorithm since times in ts are not necessarily
   * evenly spaced :( *)
  let dt = (t2 -. t1) /. float_of_int (n-1) in
  let rec loop i prev_ofs prev_t prev t =
    if i >= n then Array.of_list (List.rev prev) else
    let ofs, p = find_line fd fn sep fos t bs prev_ofs sz prev_t t_last in
    loop (i+1) ofs t (p::prev) (t +. dt) in
  loop 0 ds t_first [] t1

