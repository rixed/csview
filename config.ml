open Batteries

exception ParseError of string

(* TODO: parser from cmd line, serializer/unserializer from human editable config files
 * (use parsercombinators?) *)

type field = {
  mutable index : int ;
  mutable label : string ;
  mutable to_value : string -> float ;
  mutable to_label : string -> string ;
  mutable color : string ;
  mutable stroke_width : float ;
  mutable filled : bool ;
  mutable opacity : float ;
}

type file = {
  mutable fname : string ;
  mutable confname : string ;
  mutable fd : Unix.file_descr ;
  mutable separator : char ;
  mutable x_field : field ;
  mutable y1_fields : field array ;
  mutable y2_fields : field array ;
  mutable annot_fields : field array ;
  mutable size : int ;
  mutable first_x : float ;
  mutable last_x : float ;
  mutable block_size : int ; (* a size that worked so far *)
}

type graph = {
  mutable title : string ;
  mutable files : file array ;
  mutable x_label : string ;
  mutable y1_label : string ;
  mutable y2_label : string ;
  mutable y1_stacked : bool ;
  mutable y2_stacked : bool ;
  mutable x_start : float option ; (* initial starting position *)
  mutable x_stop : float option ;
  mutable force_show_0 : bool ;
  width : int option ;
  height : int option ;
}

type global = {
  mutable confdir : string ;
  mutable open_browser_with : string ;
  mutable default_width : int ;
  mutable default_height : int ;
}

type cli_option = {
  names : string array ; (* First one will be the one in the ini file *)
  has_param : bool ;
  descr : string ;
  doc : string ;
  setter : string -> unit ;
}

let global = {
  confdir = Unix.getenv("USER") ^ "/.csview" ;
  open_browser_with = "open http://localhost:%port%" ;
  default_width = 800 ;
  default_height = 600 ;
}

let global_options = [| {
  names = [| "confdir" |] ;
  has_param = true ;
  descr = "directory where to store the configuration files" ;
  doc = "" ;
  setter = (fun s -> global.confdir <- s) ;
} ; {
  names = [| "open-browser-with" |] ;
  has_param = true ;
  descr = "command to launch the browser" ;
  doc = "%port% will be replaced by the port csview is listening at." ;
  setter = (fun s -> global.open_browser_with <- s) ;
} ; {
  names = [| "svg-width" ; "width" |] ;
  has_param = true ;
  descr = "default graph width in pixels" ;
  doc = "" ;
  setter = (fun s -> global.default_width <- int_of_string s)
} ; {
  names = [| "svg-height" ; "height" |] ;
  has_param = true ;
  descr = "default graph height in pixels" ;
  doc = "" ;
  setter = (fun s -> global.default_height <- int_of_string s)
} |]

(* We create a new graph when we set again a field that has already been set.
 * But for stacked which cannot be compared by address. *)

let default_title = "title" and default_x_label = "X"
and default_y1_label = "Y" and default_y2_label = "Y"

let last_field = ref None

let make_new_field index = {
  index ; label = "label" ;
  to_value = float_of_string ;
  to_label = identity ;
  color = "" ;
  stroke_width = 1. ; filled = false ; opacity = 1.
}

let make_new_file fname =
  last_field := None ;
  {
    fname ; confname = "" ;
    separator = ',' ;
    fd = Unix.stdin ; (* typechecks *)
    x_field = make_new_field ~-1 ;
    y1_fields = [| |] ;
    y2_fields = [| |] ;
    annot_fields = [| |] ;
    size = -1 ;
    first_x = 0. ; last_x = 0. ;
    block_size = 1024 ;
  }

let make_new_graph () = {
  title = default_title ;
  files = [| |] ;
  x_label = default_x_label ;
  y1_label = default_y1_label ;
  y2_label = default_y2_label ;
  y1_stacked = false ; y2_stacked = false ;
  x_start = None ; x_stop  = None ;
  force_show_0 = false ;
  width = None ;
  height = None ;
}

let graphs = ref [| |]

let last_entry a = a.( Array.length a - 1 )

let append a x =
  let len = Array.length a + 1 in
  Array.init len (fun i ->
    if i = len - 1 then x else a.(i))

let get_current_graph renew =
  if Array.length !graphs = 0 ||
     renew (last_entry !graphs) then (
    let new_graph = make_new_graph () in
    graphs := append !graphs new_graph ;
    new_graph
  ) else last_entry !graphs

let no_renew _ = false

let graph_options = [| {
  names = [| "title" |] ;
  has_param = true ;
  descr = "title of that graph" ;
  doc = "" ;
  setter = (fun s ->
    let renew g = g.title != default_title in
    (get_current_graph renew).title <- s) ;
} ; {
  names = [| "x-label" |] ;
  has_param = true ;
  descr = "X axis label" ;
  doc = "" ;
  setter = (fun s ->
    let renew g = g.x_label != default_x_label in
    (get_current_graph renew).x_label <- s) ;
} ; {
  names = [| "y-label" ; "y1-label" |] ;
  has_param = true ;
  descr = "Y axis label" ;
  doc = "" ;
  setter = (fun s ->
    let renew g = g.y1_label != default_y1_label in
    (get_current_graph renew).y1_label <- s) ;
} ; {
  names = [| "y2-label" |] ;
  has_param = true ;
  descr = "right-Y axis label" ;
  doc = "" ;
  setter = (fun s ->
    let renew g = g.y2_label != default_y2_label in
    (get_current_graph renew).y2_label <- s) ;
} ; {
  names = [| "stacked" ; "y1-stacked" |] ;
  has_param = false ;
  descr = "Should values be stacked" ;
  doc = "This is only for values plotted against the left Y axis." ;
  setter = (fun s ->
    (get_current_graph no_renew).y1_stacked <- bool_of_string s) ;
} ; {
  names = [| "y2-stacked" |] ;
  has_param = false ;
  descr = "Should right values be stacked" ;
  doc = "This is only for values plotted against the right Y axis." ;
  setter = (fun s ->
    (get_current_graph no_renew).y2_stacked <- bool_of_string s) ;
} ; {
  names = [| "force-show-0" ; "force-0" ; "show-0" |] ;
  has_param = false ;
  descr = "Force the Y axis to include 0" ;
  doc = "" ;
  setter = (fun s ->
    (get_current_graph no_renew).force_show_0 <- s = "true") ;
} ; {
  names = [| "start-x" ; "x-start" ; "start" |] ;
  has_param = true ;
  descr = "Initial starting value for the X axis" ;
  doc = "First value of first file if unset." ;
  setter = (fun s ->
    (get_current_graph no_renew).x_start <- Some (float_of_string s)) ;
} ; {
  names = [| "stop-x" ; "x-stop" ; "stop" |] ;
  has_param = true ;
  descr = "Initial final value for the X axis" ;
  doc = "Last value of first file is unset." ;
  setter = (fun s ->
    (get_current_graph no_renew).x_stop <- Some (float_of_string s)) ;
} |]

(* For files we start a new one after each bareword parameter (file name) *)
let get_current_file () =
  let g = get_current_graph no_renew in
  if Array.length g.files = 0 then
    raise (ParseError "Must give the file name before the file options") ;
  last_entry g.files

let new_file s =
  let g = get_current_graph no_renew in
  let f = make_new_file s in
  g.files <- append g.files f

let file_options = [| {
  names = [| "separator" |] ;
  has_param = true ;
  descr = "character to use as field separator" ;
  doc = "" ;
  setter = (fun s ->
    if String.length s != 1 then
      invalid_arg "separator must be a single character" ;
    (get_current_file ()).separator <- s.[0]) ;
} ; {
  names = [| |] ;
  has_param = false ;
  descr = "CSV file" ;
  doc = "" ;
  setter = (fun s -> new_file s) ;
} ; {
  names = [| "block-size" |] ;
  has_param = true ;
  descr = "any block of that size should contain at least a full line." ;
  doc = "" ;
  setter = (fun s ->
    (get_current_file ()).block_size <- int_of_string s) ;
} ; {
  names = [| "max-line-length" ; "max-line-len" |] ;
  has_param = true ;
  descr = "max line length" ;
  doc = "Including the newline character." ;
  setter = (fun s ->
    (get_current_file ()).block_size <- 2 * int_of_string s) ;
} ; {
  names = [| "confname" ; "conf-name" ; "config-name" |] ;
  has_param = true ;
  descr = "assume this file name for fetching/saving configuration" ;
  doc = "" ;
  setter = (fun s ->
    (get_current_file ()).confname <- s) ;
} |]

(* We create a new field each time we set a value that was already set *)

let get_last_field () =
  match !last_field with
  | None -> raise (ParseError "Cannot set a value for a field before giving its index")
  | Some f -> f

let get_current_x_field () =
  let f = (get_current_file ()).x_field in
  last_field := Some f ;
  f

let new_field fields idx =
  assert (idx >= 0) ;
  let f = make_new_field idx in
  last_field := Some f ;
  append fields f

(* TODO: pass also a range of possible values so that we know what
 * accuracy is required! *)
let string_of_timestamp ts =
  let open Unix in
  let tm = localtime (float_of_string ts) in
  Printf.sprintf "%04d-%02d-%02d %02d:%02d:%02d"
    (tm.tm_year + 1900) (tm.tm_mon + 1) tm.tm_mday
    tm.tm_hour tm.tm_min tm.tm_sec

let field_options = [| {
  names = [| "x" |] ;
  has_param = true ;
  descr = "field number (starting at 0) of the X value for this graph" ;
  doc = "" ;
  setter = (fun s ->
    Printf.eprintf "set x index to %s\n%!" s ;
    let f = get_current_x_field () in
    if f.index <> ~-1 then
      raise (ParseError "Set twice the X field") ;
    f.index <- int_of_string s) ; (* TODO: index of string that validates >= 0 *)
} ; {
  names = [| "y" ; "y1" |] ;
  has_param = true ;
  descr = "field number of the next value reported on the left Y axis" ;
  doc = "" ;
  setter = (fun s ->
    let idx = int_of_string s in
    let file = get_current_file () in
    file.y1_fields <- new_field file.y1_fields idx) ;
} ; {
  names = [| "y2" |] ;
  has_param = true ;
  descr = "field number of the next value reported on the right Y axis" ;
  doc = "" ;
  setter = (fun s ->
    let idx = int_of_string s in
    let file = get_current_file () in
    file.y2_fields <- new_field file.y2_fields idx) ;
} ; {
  names = [| "annot" ; "y3" |] ;
  has_param = true ;
  descr = "field number of the next value to use as annotation" ;
  doc = "" ;
  setter = (fun s ->
    let idx = int_of_string s in
    let file = get_current_file () in
    file.annot_fields <- new_field file.annot_fields idx) ;
} ; {
  names = [| "label" |] ;
  has_param = true ;
  descr = "label for this field" ;
  doc = "" ;
  setter = (fun s -> (get_last_field ()).label <- s)
} ; {
  names = [| "color" ; "col" |] ;
  has_param = true ;
  descr = "color to use for this field" ;
  doc = "" ;
  setter = (fun s -> (get_last_field ()).color <- s)
} ; {
  names = [| "stroke-width" |] ;
  has_param = true ;
  descr = "stroke width" ;
  doc = "" ;
  setter = (fun s -> (get_last_field ()).stroke_width <- float_of_string s)
} ; {
  names = [| "opacity" |] ;
  has_param = true ;
  descr = "opacity" ;
  doc = "" ;
  setter = (fun s -> (get_last_field ()).opacity <- float_of_string s)
} ; {
  names = [| "filled" ; "fill" |] ;
  has_param = false ;
  descr = "should the area below this value be filled?" ;
  doc = "" ;
  setter = (fun s -> (get_last_field ()).filled <- bool_of_string s)
} ; {
  names = [| "format" |] ;
  has_param = true ;
  descr = "numeric|timestamp|date(...a la strftime...)" ;
  doc = "" ;
  setter = (fun s ->
    let f = get_last_field () in
    match s with
    | "numeric" ->
      f.to_value <- float_of_string ;
      f.to_label <- identity
    | "timestamp" ->
      f.to_value <- float_of_string ;
      f.to_label <- string_of_timestamp
    | _ ->
      invalid_arg s)
} |]

(* Other options are just for this run and not backed by any config file *)

let open_browser = ref true
let print_help = ref false

let other_options = [| {
  names = [| "open-browser" ; "open" |] ;
  has_param = false ;
  descr = "automatically launch the browser" ;
  doc = "You may need to use the --opan-browser-with option to configure\
         the details." ;
  setter = (fun v -> open_browser := bool_of_string v) ;
} ; {
  names = [| "help" |] ;
  has_param = false ;
  descr = "print this help" ;
  doc = "" ;
  setter = (fun _ -> print_help := true) ;
} |]

(*
 * Command Line
 *)

let chop s n =
  String.sub s n (String.length s - n)

(*$= chop & ~printer:identity
  "glop" (chop "pas glop" 4)
  "glop" (chop "glop" 0)
  ""     (chop "glop" 4)
 *)

let chop_dashes s =
  if String.starts_with s "--" then chop s 2
  else if String.starts_with s "-" then chop s 1
  else raise Not_found

(*$= chop_dashes & ~printer:identity
  "opt"  (chop_dashes "--opt")
  "opt"  (chop_dashes "-opt")
  "-opt" (chop_dashes "---opt")
  "exc"  (try chop_dashes "opt" with Not_found -> "exc")
 *)

let try_parse_option n v opts =
  match chop_dashes n with
  | exception Not_found ->
    None
  | n ->
    let rec loop o =
      if o >= Array.length opts then None else
      let opt = opts.(o) in
      if Array.exists ((=) n) opt.names then
        match opt.has_param, v with
        | true, Some v ->
          Some (opt, v)
        | false, _ ->
          Some (opt, "true")
        | _ -> None
      else if not opt.has_param &&
        String.starts_with n "no" &&
        Array.exists ((=) (chop_dashes (chop n 2))) opt.names then
        Some (opt, "false")
      else loop (o+1) in
    loop 0

let try_parse_bareword opts =
  try Some (Array.find (fun opt ->
      Array.length opt.names = 0
    ) opts)
  with Not_found -> None

let parse_options options args =
  let rec loop i =
    if i < Array.length args then (
      match args.(i) with
      | None -> loop (i+1)
      | Some p ->
        let v =
          if i < Array.length args - 1 then args.(i+1)
          else None in
        try (
          let opt, v =
            try List.find_map (try_parse_option p v) options
            with Not_found ->
              List.find_map try_parse_bareword options, p in
          opt.setter v ;
          args.(i) <- None ;
          if opt.has_param then args.(i+1) <- None ;
          loop (i + (if opt.has_param then 2 else 1))
        ) with Not_found -> loop (i + 1)
    ) in
  loop 1

let parse_args args =
  (* We want the global and runtime options to be parsed first, and be
   * insensitive to their position in the command line, and then the other
   * options which effect depend on position. *)
  (* So we can remove options that are processed already: *)
  let args = Array.map Option.some args in
  args.(0) <- None ;  (* This is not an option *)
  parse_options [ other_options ] args ;
  parse_options [ global_options ] args ;
  parse_options [ graph_options ; file_options ; field_options ] args ;
  if !print_help then (
    List.iter (fun (section, opts) ->
      Printf.printf "%s\n\n" section ;
      Array.iter (fun o ->
          Array.iteri (fun i n ->
              Printf.printf "%s--%s" (if i > 0 then ", " else "") n
            ) o.names ;
          Printf.printf "\n  %s\n" o.descr ;
          if o.doc <> "" then (
            Printf.printf "\n  %s\n" o.doc
          ) ;
          Printf.printf "\n"
        ) opts ;
      Printf.printf "\n") [
        "Runtime options", other_options ;
        "Global options", global_options ;
        "Graph options", graph_options ;
        "File options", file_options ;
        "Field options", field_options ] ;
    exit 0
  ) ;
  Array.iter (function
    | None -> ()
    | Some arg ->
      Printf.eprintf "Cannot parse '%s'\n" arg ;
      exit 1) args
