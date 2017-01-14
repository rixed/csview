open Batteries

(* Converts from csv-string to floats, and from csv-string to label-strings *)

type t = {
  to_value : string -> float ;
  to_label : float -> string ;
  name : string ;
}

let numeric = {
  name = "numeric" ;
  to_value = float_of_string ;
  to_label = Html.my_string_of_float ;
}

(* TODO: reset this state after each axis... *)
let last_s = ref ""

let timestamp = {
  name = "timestamp" ;
  to_value = float_of_string ;
  to_label = fun t ->
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
    let pl = if pl < 5 then 0 else
             if pl < 8 then 5 else 
             if pl < 11 then 8 else
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
}

let reset_all_states () = last_s := ""


let all =
  [ numeric ; timestamp ]
