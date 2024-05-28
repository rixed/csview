open Batteries

(* Converts from csv-string to floats, and from csv-string to label-strings *)

type t = {
  to_value : string -> float ;
  to_label : float -> string ;
  name : string ;
  base : float ;
}

let numeric _ = {
  name = "numeric" ;
  base = 10. ;
  to_value = (fun s ->
    try float_of_string s
    with Failure _ ->
      Printf.eprintf "Cannot convert value %S into numeric\n" s ;
      exit ~-1) ;
  to_label = Html.my_string_of_float ;
}

(* reset after each axis... *)
let last_s = ref ""

let label_of_timestamp t =
  let open Unix in
  let tm = localtime t in
  let s = Printf.sprintf "%04d-%02d-%02d %02dh%02dm%02ds"
            (tm.tm_year + 1900) (tm.tm_mon + 1) tm.tm_mday
            tm.tm_hour tm.tm_min tm.tm_sec in
  let rec prefix_len i =
    if i >= String.length !last_s ||
       s.[i] <> !last_s.[i] then i
    else prefix_len (i+1) in
  let pl = prefix_len 0 in
  (* Avoid axing the string in the middle of a meaningful unit: *)
  let pl = if pl < 11 then 0 else
           if pl < 14 then 11 else
           if pl < 17 then 14 else
           if pl < 20 then 17 else pl in
  last_s := s ;
  (* Make it even shorter by removing 00 endings: *)
  let rec suffix_start i =
    let zero = "XXXX-XX-XX 00h00m00s" in
    if zero.[i] <> s.[i] then i
    else suffix_start (i-1) in
  let e = suffix_start (String.length s - 1) in
  (* Avoid chopping the unit: *)
  let e = if e > 16 then 19 else
          if e > 13 then 16 else
          if e > 10 then 13 else e in
  String.sub s pl (e + 1 - pl)

let timestamp _ = {
  name = "timestamp" ;
  base = 60. ;
  to_value = (fun s ->
    try float_of_string s
    with Failure _ ->
      Printf.eprintf "Cannot convert value %S into timestamp\n" s ;
      exit ~-1) ;
  to_label = label_of_timestamp 
}

(* This one comes with a parameter string giving the actual format *)
type my_tm = { mutable sec : int ; mutable min : int ; mutable hour : int ;
               mutable mday : int ; mutable mon : int ; mutable year : int }
let tm_of_my_tm tm =
  Unix.{ tm_sec = tm.sec ; tm_min = tm.min ; tm_hour = tm.hour ;
         tm_mday = tm.mday ; tm_mon = tm.mon - 1 ; tm_year = tm.year - 1900 ;
         tm_wday = 0 ; tm_yday = 0 ; tm_isdst = false }

exception BadDateFormat of string
exception BadDateValue of string

let strptime fmt str =
  let digits n s o =
    let rec loop o n d =
      if n = 0 then d else (
        if o >= String.length s then
          raise (BadDateValue (
            "Missing digits at end of date value '"^ str ^"'")) ;
        let c = s.[o] in
        let c = if c = ' ' then '0' else c in
        if c >= '0' && c <= '9' then
          loop (o+1) (n-1) (d*10 + Char.code c - Char.code '0')
        else
          raise (BadDateValue (
            "Cannot read a digit at position "^ string_of_int o ^
            " of '"^ s ^"'"))
      ) in
    loop o n 0 in
  let parse_single tm s o = function
    | 'd' | 'e' -> tm.mday <- digits 2 s o ; o+2
    | 'H' | 'k' | 'l' | 'I' -> tm.hour <- digits 2 s o ; o+2
    | 'j' -> tm.mon <- 1 ;
             tm.mday <- digits 3 s o ; (* will be normalized by mktime *)
             o+3
    | 'M' -> tm.min <- digits 2 s o ; o+2
    | 'm' -> tm.mon <- digits 2 s o ; o+2
    | 'S' -> tm.sec <- digits 2 s o ; o+2
    | 'Y' -> tm.year <- digits 4 s o ; o+4
    | 'y' -> tm.year <- 1900 + digits 2 s o ; o+2
    | c -> raise (BadDateFormat (
             "Unknown format specifier '"^ String.of_char c ^"'")) in
  let parse_lit s o chunk c =
    let fix_len = String.length chunk - c in
    if String.length s < o + fix_len then
      raise (BadDateValue (
        "Missing chars ("^ String.of_char chunk.[c] ^"...) at end of date value '"^ s ^"'")) ;
    for i = 0 to fix_len - 1 do
      if s.[o + i] <> chunk.[c + i] then
        raise (BadDateValue (
          "Cannot match format char '"^ String.of_char chunk.[c+i] ^"'\
           in date value '"^ s ^"' at position "^ string_of_int (o+i)))
    done ;
    o + fix_len in
  let parse_chunk tm s (first, o) chunk =
    (* The first chunk (first=true) is just a literal to appear on the value.
     * The other chunks starts with a format character, and the rest is the
     * literal.  Empty string can happen at the beginning of the format. *)
    false, if first then (
      if chunk = "" then o else parse_lit s o chunk 0
    ) else (
      let o = parse_single tm s o chunk.[0] in
      parse_lit s o chunk 1
    ) in
  let tm = { sec = 0 ; min = 0 ; hour = 0 ; mday = 0 ; mon = 0 ; year = 0 } in
  let chunks = String.split_on_char '%' fmt in
  let _, o = List.fold_left (parse_chunk tm str) (true,0) chunks in
  if o <> String.length str then
    raise (BadDateValue (
      "Spurious chars at end of date value '"^ str ^"' at position "^
      string_of_int o)) ;
  let tm' = tm_of_my_tm tm in
  Unix.mktime tm' |> fst
(*$= strptime & ~printer:string_of_float
  1484504510. (strptime "%Y-%m-%d %H:%M:%S" "2017-01-15 19:21:50")
  1484504510. (strptime "%m/%d/%Y %H:%M:%S" "01/15/2017 19:21:50")
  1484504510. (strptime "glop %Y-%m-%d glop %H:%M:%S pas glop" \
                        "glop 2017-01-15 glop 19:21:50 pas glop")
  1484434800. (strptime "%Y+%j" "2017+015")
 *)

let date fmt = {
  name = "date("^ fmt ^")" ;
  base = 60. ;
  to_label = label_of_timestamp ;
  to_value = fun s ->
    strptime fmt s
}

let reset_all_states () = last_s := ""

let all =
  [ "numeric", numeric ;
    "timestamp", timestamp ;
    "date", date ]
