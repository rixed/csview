open Batteries

let max_line_size_ever = 10240

let file_size fd = Unix.(lseek fd 0 SEEK_END)

(*$= file_size & ~printer:string_of_int
  11020 (file_size test_data_1_csv)
 *)

let read_at fd ofs bs =
  Printf.eprintf "read %d bytes @ %d\n%!" bs ofs ;
  let open Unix in
  let buf = Bytes.create bs in
  let ofs' = lseek fd ofs SEEK_SET in
  assert (ofs' = ofs) ;
  let r = read fd buf 0 bs in
  assert (r = bs) ;
  Bytes.to_string buf

(* Laisser tomber ce reader etc. Utiliser directement un Config.file.  Ajouter
 * dans graph.files le file descriptor et le premier x, le dernier et l'offset
 * max (ces deux dernier mutable).
 * 
 * Note: on a plusieurs file par graph, avec des ts propres (et pas forcement
 * alignés). Mais pour grapher on veux aligner les X (stacked graph, etc).
 * Bref, on a fait un truc trop compliqué dans Config -> on veux un ficher par
 * graph!  Apres tout on peut toujours joindre les CSV avant (comme filtres et
 * projections, on peut supposer qu'on  a un beau CSV en input).  Refactorer
 * config.ml, donc
 *
 * Non, parceque au moins pour les annotations on a besoin de plusieurs
 * fichiers. En fait on pourrait afficher les fichiers comme des graphs
 * indépendants, sans chercher å synchroniser (comme pour les annotations,
 * pour lesquelles il est important de concerver le vrai temp). Le seul soucis
 * c'est d'avoir plein de labels sur les axes. On pourrait donc dire qu'on ne
 * trace les axes et leur labels que pour la première courbe et les autre par
 * dessus sans labels. Oui voilà!
 *)

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
let extract_field str field_sep line_start field_num fos =
  let field_start =
    find_nth_field_from str field_sep line_start field_num in
  let field_stop = sep_index_from str field_sep field_start in
  String.sub str field_start (field_stop - field_start) |> fos,
  field_stop

(* returns -1,0 or -1, as well as the offset in the block where the line starts
 * and the value for the time. Raises Not_found if we cannot find any value of
 * t.  Only looks at the first line within the block. *)
let cmp_block_begin str field_num field_sep float_of_field target =
  (* Look for the first line start *)
  let line_start = String.index str '\n' + 1 in
  let t, _ =
    extract_field str field_sep line_start field_num float_of_field in
  Printf.eprintf "found ts=%f @ %d\n%!" t line_start ;
  Float.compare target t

(* Same as above but look for all values in that block ; returns the offset of
 * the line that's the closest to the value we are looking for. It's OK if
 * that line is not complete but we must consider only the lines which time
 * _is_ fully present. *)
let find_line_in_block str field_num field_sep float_of_field target =
  (* Note: this does not work for the first line of the file... *)
  let line_start = String.index str '\n' + 1 in
  let rec loop (prev_t, _prev_ofs as prev) ofs =
    match extract_field str field_sep ofs field_num float_of_field with
    | exception Not_found ->
      (* So the best we have is the previous one.
       * Note: It is guaranteed that we have had a full line already *)
      prev
    | t, field_stop ->
      let prev' = t, ofs in
      if t < target then (
        match String.index_from str field_stop '\n' with
        | exception Not_found ->
          (* So that was it *)
          prev'
        | end_of_line ->
          loop prev' (end_of_line+1)
      ) else (
        (* We passed the target *)
        if ofs = line_start then (
          t, line_start
        ) else (
          let d_before = target -. prev_t (* Will never happen on first iteration of loop because of starting value *)
          and d_after = t -. target in
          if d_before <= d_after then prev else prev'
        )
      ) in
  loop ((* won't be used I promise *) 0., 0) line_start

(* Now given a block reader we can find the line with the target timestamp.
 * ofs_start is such that this beginning of a line is <= target and
 * ofs_end may be anywhere in a line that's > target *)
let rec find_block fd field_num field_sep float_of_field target block_size_max ofs_start ofs_stop =
  Printf.eprintf "find in [%d;%d]\n%!" ofs_start ofs_stop ;
  if ofs_stop - ofs_start <= block_size_max then ofs_start else (
    let ofs_mid = (ofs_stop + ofs_start) / 2 in
    assert (ofs_mid >= ofs_start) ;
    let block_size = min block_size_max (ofs_stop - ofs_mid) in
    assert (ofs_mid + block_size <= ofs_stop) ;
    let str = read_at fd ofs_mid block_size in
    match cmp_block_begin str field_num field_sep float_of_field target with
    | -1 ->
      find_block fd field_num field_sep float_of_field target block_size_max ofs_start ofs_mid
    | 1 ->
      find_block fd field_num field_sep float_of_field target block_size_max ofs_mid ofs_stop
    | _ ->
      ofs_mid)

let find_line fd field_num field_sep float_of_field target block_size_max max_ofs =
  let ofs = find_block fd field_num field_sep float_of_field target block_size_max 0 max_ofs in
  let block_size = min block_size_max (max_ofs - ofs) in
  let str = read_at fd ofs block_size in
  Printf.eprintf "find in block @ %d + %d\n%!" ofs block_size ;
  let t_line, line_start = find_line_in_block str field_num field_sep float_of_field target in
  let line_stop = String.index_from str line_start '\n' in
  (* we want the end of line char to be present as a delimiter *)
  t_line, String.sub str line_start (line_stop - line_start + 1)


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
    Printf.eprintf "can't find nl\n%!" ;
    (* try with bigger block size *)
    assert (bs < max_line_size_ever) ;
    get_last_x fd sz (bs * 2) sep fn fos
  | eol ->
    Printf.eprintf "found eol at %d\n%!" eol ;
    (* we must found the beginning of that line *)
    (match String.rindex_from str (eol-1) '\n' with
    | exception (Invalid_argument _ (* eol-1 was -1 *) | Not_found) ->
      if bs' < sz then (
        Printf.eprintf "can't find sol\n%!" ;
        assert (bs < max_line_size_ever) ;
        get_last_x fd sz (bs * 2) sep fn fos
      ) else (
        (* we are at the beginning of the file *)
        Printf.eprintf "we are at beginning of file\n%!" ;
        extract_field str sep 0 fn fos |> fst
      )
    | last_eol ->
      let sol = last_eol + 1 in
      Printf.eprintf "found sol at %d\n%!" sol ;
      extract_field str sep sol fn fos |> fst)

(*$= get_last_x & ~printer:string_of_float
   1483380648. (get_last_x test_data_1_csv 11020 4096 ',' 0 float_of_string)
 *)

let rec get_first_x fd sz bs sep fn fos =
  let bs' = min sz bs in
  let str = read_at fd 0 bs' in
  try extract_field str sep 0 fn fos |> fst
  with Not_found ->
    assert (bs < max_line_size_ever) ;
    get_first_x fd sz (bs * 2) sep fn fos

(*$= get_first_x & ~printer:string_of_float
   1483347348. (get_first_x test_data_1_csv 11020 4096 ',' 0 float_of_string)
 *)

(* Refresh a config file info *)
let update_file_info f =
  let open Config in
  Printf.eprintf "  Check file %s...\n%!" f.fname ;
  let sz = file_size f.fd in
  if sz <> f.size then (
    f.size <- sz ;
    Printf.eprintf "    size is now %d\n%!" sz ;
    (try
      f.last_x <-
        get_last_x f.fd f.size f.block_size f.separator f.x_field.index f.x_field.to_value ;
      Printf.eprintf "    last x is now %f\n%!" f.last_x
     with Not_found -> ()) ;
    if f.first_x = 0. then
      (try
        f.first_x <-
          get_first_x f.fd f.size f.block_size f.separator f.x_field.index f.x_field.to_value ;
        Printf.eprintf "    first x is now %f\n%!" f.first_x
       with Not_found -> ()) ;
  )

(* We must return no more than n indices but we can return less if we do have
 * enough data (avoid sending several times the same index!). We also want to
 * have indices evenly spaced in time (not in the index space!) therefore we
 * could also return less than n indices if we have more than n times in
 * between t1 and t2 but they are clustered. *)
(* FIXME: we do not use the info that t' = t + dt / should be useful when bisecting! aka, pass a hint to find_line! *)
let read_all fd fn sep fos bs sz n t1 t2 =
  if n = 0 || t1 > t2 then [||] else
  if n = 1 then let tmid = (t2 -. t1) *. 0.5 in
    [| find_line fd fn sep fos tmid bs sz |] else
  (* Impossible to use fancy algorithm since times in ts are not necessarily
   * evenly spaced :( *)
  let dt = (t2 -. t1) /. float_of_int (n-1) in
  let rec loop prev t =
    if t > t2 then Array.of_list (List.rev prev) else
    let x, _ as p = find_line fd fn sep fos t bs sz
    and t' = t +. dt in
    (* Avoid adding several times the same X - in case of oversampling *)
    match prev with
    | (x',_)::_ when x' = x ->
      loop prev t'
    | prev ->
      loop (p::prev) t' in
  loop [] t1

