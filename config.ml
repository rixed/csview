open Batteries

exception ParseError of string

type pen = {
  mutable color : Color.t ;
  mutable color_was_set : bool ;
  mutable opacity : float ;
  mutable stroke_width : float ;
  mutable filled : bool ;
  mutable fill_opacity : float ;
}

type field = {
  mutable index : int ;
  mutable label : string ;
  mutable fmt : Formats.t ;
  mutable fmt_was_set : bool ; (* track those that were explicitly set *)
  mutable pen : pen ;
}

type file = {
  mutable fname : string ;
  mutable confname : string ;
  mutable fd : Unix.file_descr ;
  mutable separator : char ;
  mutable has_header : bool ;
  mutable x_field : field ;
  mutable y1_fields : field array ;
  mutable y2_fields : field array ;
  mutable annot_fields : field array ;
  mutable data_start : int ;  (* not 0 if has_header *)
  mutable size : int ;
  mutable first_x : float ;
  mutable last_x : float ;
  mutable block_size : int ; (* a size that worked so far *)
}

type stacked = NotStacked | Stacked | StackedCentered
type legend_location = NoShow
                     | UpperLeft | UpperRight | BottomLeft | BottomRight
                     | Absolute of float * float

let string_of_legend = function
  | NoShow -> "none"
  | UpperLeft -> "upperleft"
  | UpperRight -> "upperright"
  | BottomLeft -> "bottomleft"
  | BottomRight -> "bottomright"
  | Absolute (x, y) -> Printf.sprintf "%f,%f" x y

let legend_location_of_string str =
  match String.lowercase str with
  | "none" -> NoShow
  | "upperleft" -> UpperLeft
  | "upperright" -> UpperRight
  | "bottomleft" -> BottomLeft
  | "bottomright" -> BottomRight
  | _ ->
    try Scanf.sscanf str "%f,%f" (fun x y -> Absolute (x, y))
    with Scanf.Scan_failure _ ->
      raise (ParseError ("Cannot parse legend location '"^ str ^"'"))

type graph = {
  mutable title : string ;
  mutable files : file array ;
  mutable y1_label : string ;
  mutable y2_label : string ;
  mutable y1_stacked : stacked ;
  mutable y2_stacked : stacked ;
  mutable x_start : float option ; (* initial starting position *)
  mutable x_stop : float option ;
  mutable force_show_0 : bool ;
  mutable font_size : float ;
  mutable draw_legend : legend_location ;
  mutable draw_legend_was_set : bool ;
  width : int option ;
  height : int option ;
}

type global = {
  mutable confdir : string ;
  mutable save_config : bool ;
  mutable open_browser_with : string ;
  mutable default_width : int ;
  mutable default_height : int ;
}

(* Config file: super hackish! Should be rewritten, with each parameter (above)
 * having a getter that either take a value set by command line or fallback to
 * the configuration file(s). *)

let mkdir_all ?(is_file=false) dir =
    let dir_exist d =
        try Sys.is_directory d with Sys_error _ -> false in
    let dir = if is_file then Filename.dirname dir else dir in
    let rec ensure_exist d =
        if String.length d > 0 && not (dir_exist d) then (
            ensure_exist (Filename.dirname d) ;
            try Unix.mkdir d 0o755
            with Unix.Unix_error (Unix.EEXIST, "mkdir", _) ->
                (* Happen when we have "somepath//someother" (dirname should handle this IMHO *)
                ()
        ) in
    ensure_exist dir

let with_save_file confdir name =
  mkdir_all confdir ;
  let fname = confdir ^"/"^ name in
  Printf.eprintf "Storing config in file '%s'\n" fname ;
  let mode = [ `create; `trunc; `text ] in
  File.with_file_out ~mode fname

let params_of_file confdir name =
  let fname = confdir ^"/"^ name in
  File.lines_of fname |>
  Enum.map Option.some |>
  Array.of_enum

let save_field_config confdir field =
  let fname = string_of_int field.index in
  with_save_file confdir fname (fun oc ->
    Printf.fprintf oc "--label\n%s\n\
                       --format\n%s\n\
                       %s\
                       --color\n%s\n\
                       --opacity\n%f\n\
                       --fill-opacity\n%f\n"
              field.label field.fmt.Formats.name
              (if field.pen.filled then "--filled\n" else "")
              (Color.to_string field.pen.color)
              field.pen.opacity
              field.pen.fill_opacity)

let file_confname file =
  (if file.confname <> "" then file.confname else file.fname) |>
  Filename.basename

let params_of_field_config confdir file n =
  let confdir = confdir ^"/files/"^ file_confname file ^".fields" in
  try params_of_file confdir (string_of_int n)
  with exn ->
    Printf.eprintf "Cannot read field %d configuration from %s: %s\n"
      n confdir (Printexc.to_string exn) ;
    [| |]

let save_file_config confdir file =
  let confdir = confdir ^"/files" in
  let fname = file_confname file in
  with_save_file confdir fname (fun oc ->
    (* For a file we want to save the separator, the block size, and for each
     * possible field (indexed by field number) its label, format, and
     * graphic config. This does not say what should be ultimately used as
     * a graph Y or X value, though. This goes in the graph savefile.
     * In order to easily update a file config we store each field in a
     * separate file. Inodes are cheap!.*)
    Printf.fprintf oc "--separator\n%c\n\
                       --block-size\n%d\n"
      file.separator file.block_size ;
    if file.has_header then Printf.fprintf oc "--has-header\n" ;
    ) ;
  let file_confdir = confdir ^"/"^ fname ^".fields" in
  save_field_config file_confdir file.x_field ;
  Array.iter (save_field_config file_confdir) file.y1_fields ;
  Array.iter (save_field_config file_confdir) file.y2_fields

let params_of_file_config confdir file =
  let confdir = confdir ^"/files/"^ file_confname file in
  try params_of_file confdir (file_confname file)
  with exn ->
    Printf.eprintf "Cannot read file configuration from %s: %s\n"
      confdir (Printexc.to_string exn) ;
    [| |]

let to_fname s = String.nreplace ~str:s ~sub:"/" ~by:"_"

let string_of_stacked what = function
  | NotStacked -> "--no-"^ what ^"-stacked"
  | Stacked -> "--"^ what ^"-stacked"
  | StackedCentered -> "--"^ what ^"-stackedcentered"

let print_opt_float oc label = function
  | None -> ()
  | Some x -> Printf.fprintf oc "%s\n%f\n" label x

let print_opt_int oc label = function
  | None -> ()
  | Some x -> Printf.fprintf oc "%s\n%d\n" label x

let save_graph_config confdir graph =
  with_save_file (confdir ^"/graphs") (to_fname graph.title) (fun oc ->
    Printf.fprintf oc "--y1-label\n%s\n\
                       --y2-label\n%s\n\
                       %s\n%s\n\
                       --font-size\n%f\n\
                       --legend\n%s\n"
      graph.y1_label graph.y2_label
      (string_of_stacked "y1" graph.y1_stacked)
      (string_of_stacked "y2" graph.y2_stacked)
      graph.font_size
      (string_of_legend graph.draw_legend) ;
    print_opt_float oc "--x-start" graph.x_start ;
    print_opt_float oc "--x-stop" graph.x_stop ;
    if graph.force_show_0 then Printf.fprintf oc "--force-show-0\n" ;
    print_opt_int oc "--width" graph.width ;
    print_opt_int oc "--height" graph.height ;
    Array.iter (fun file ->
      Printf.fprintf oc "%s\n" file.fname ;
      if file.confname <> "" then
        Printf.fprintf oc "--confname\n%s\n" file.confname ;
      save_file_config confdir file) graph.files)

let params_of_graph_config confdir g =
  let confdir = confdir ^"/graphs" in
  try params_of_file confdir (to_fname g.title)
  with exn ->
    Printf.eprintf "Cannot read graph configuration from %s: %s\n"
      confdir (Printexc.to_string exn) ;
    [| |]


let save_global_config confdir global =
  with_save_file confdir "global" (fun oc ->
    Printf.fprintf oc "--open-browser-with\n%s\n\
                       --default-width\n%d\n\
                       --default-height\n%d\n"
      global.open_browser_with
      global.default_width
      global.default_height)

type cli_option = {
  names : string array ; (* First one will be the one in the ini file *)
  has_param : bool ;
  descr : string ;
  doc : string ;
  setter : string -> unit ;
}

let global = {
  confdir =
    (let homedir =
      try Unix.getenv("HOME")
      with Not_found -> "/tmp" in
    homedir ^"/.csview") ;
  save_config = false ;
  open_browser_with = CompilConfig.default_open ^" http://localhost:%port%" ;
  default_width = 800 ;
  default_height = 600 ;
}

let global_options = [| {
  names = [| "confdir" |] ;
  has_param = true ;
  descr = "directory where configuration files are stored" ;
  doc = "Where to read and optionally write settings." ;
  setter = (fun s -> global.confdir <- s) ;
} ; {
  names = [| "save-config" ; "save" |] ;
  has_param = false ;
  descr = "if set, save file/graph configuration" ;
  doc = "Saved configuration is associated with file/graph names and will\
         later be reused as default for the same files/graphs." ;
  setter = fun s -> global.save_config <- bool_of_string s
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

let default_title = "title"
and default_y_label = "Y"

let last_field = ref None
let default_label = "label"

let make_new_field index = {
  index ; label = default_label ;
  fmt = Formats.numeric "" ;
  fmt_was_set = false ;
  pen = {
    color = [| 0. ; 0. ; 0. |] ;
    color_was_set = false ;
    opacity = 1. ;
    stroke_width = 1. ;
    filled = false ;
    fill_opacity = 0.7 ;
  }
}

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
        | true, None ->
          Printf.eprintf "Missing parameter for option '%s'\n" n ;
          exit ~-1
        | false, _ ->
          Some (opt, "true")
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

let make_new_file fname =
  last_field := None ;
  {
    fname ; confname = "" ;
    separator = ',' ;
    has_header = false ;
    fd = Unix.stdin ; (* typechecks *)
    x_field = make_new_field ~-1 ;
    y1_fields = [| |] ;
    y2_fields = [| |] ;
    annot_fields = [| |] ;
    data_start = 0; size = -1 ;
    first_x = 0. ; last_x = 0. ;
    block_size = 1024 ;
  }

let make_new_graph () = {
  title = default_title ;
  files = [| |] ;
  y1_label = default_y_label ;
  y2_label = default_y_label ;
  y1_stacked = NotStacked ;
  y2_stacked = NotStacked ;
  x_start = None ; x_stop  = None ;
  force_show_0 = false ;
  font_size = 14. ;
  draw_legend = UpperRight ;
  draw_legend_was_set = false ;
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

(* For files we start a new one after each bareword parameter (file name) *)
let get_current_file () =
  let g = get_current_graph no_renew in
  if Array.length g.files = 0 then
    raise (ParseError "Must give the file name before the file options") ;
  last_entry g.files

let rec load_graph_config confdir g =
  let args = params_of_graph_config confdir g in
  parse_options [ graph_options ; file_options ] args

and graph_options = [| {
  names = [| "title" |] ;
  has_param = true ;
  descr = "title of that graph" ;
  doc = "" ;
  setter = fun s ->
    let renew g = g.title != default_title in
    let g = get_current_graph renew in
    g.title <- s ;
    load_graph_config global.confdir g
} ; {
  names = [| "y-label" ; "y1-label" |] ;
  has_param = true ;
  descr = "Y axis label" ;
  doc = "" ;
  setter = (fun s ->
    let renew g = g.y1_label != default_y_label in
    (get_current_graph renew).y1_label <- s) ;
} ; {
  names = [| "y2-label" |] ;
  has_param = true ;
  descr = "right-Y axis label" ;
  doc = "" ;
  setter = (fun s ->
    let renew g = g.y2_label != default_y_label in
    (get_current_graph renew).y2_label <- s) ;
} ; {
  names = [| "stacked" ; "y1-stacked" |] ;
  has_param = false ;
  descr = "Should values be stacked" ;
  doc = "This is only for values plotted against the left Y axis." ;
  setter = (fun s ->
    (get_current_graph no_renew).y1_stacked <-
      if bool_of_string s then Stacked else NotStacked) ;
} ; {
  names = [| "stackcentered" ; "stackedcentered" ; "stackedcenter" ;
             "y1-stackcentered" ; "y1-stackedcentered" ;
             "y1-stackedcenter" |] ;
  has_param = false ;
  descr = "Should values be stacked (smarter stacking)" ;
  doc = "This is only for values plotted against the left Y axis." ;
  setter = (fun s ->
    (get_current_graph no_renew).y1_stacked <-
      if bool_of_string s then StackedCentered else NotStacked) ;
} ; {
  names = [| "y2-stacked" |] ;
  has_param = false ;
  descr = "Should right values be stacked" ;
  doc = "This is only for values plotted against the right Y axis." ;
  setter = (fun s ->
    (get_current_graph no_renew).y2_stacked <-
      if bool_of_string s then Stacked else NotStacked) ;
} ; {
  names = [| "stackedcentered" ; "stackedcenter" ; "y1-stackcentered" ;
             "y1-stackedcentered" ; "y1-stackedcenter" |] ;
  has_param = false ;
  descr = "Should right values be stacked (smarter stacking)" ;
  doc = "This is only for values plotted against the right Y axis." ;
  setter = (fun s ->
    (get_current_graph no_renew).y2_stacked <-
      if bool_of_string s then StackedCentered else NotStacked) ;
} ; {
  names = [| "force-show-0" ; "force-0" ; "show-0" |] ;
  has_param = false ;
  descr = "Force the Y axis to include 0" ;
  doc = "" ;
  setter = (fun s ->
    (get_current_graph no_renew).force_show_0 <- s = "true") ;
} ; {
  names = [| "font-size" |] ;
  has_param = true ;
  descr = "font size" ;
  doc = "" ;
  setter = fun s ->
    (get_current_graph no_renew).font_size <- float_of_string s
} ; {
  names = [| "legend" ; "show-legend" ; "legend-position" ;
             "legend-location" |] ;
  has_param = true ;
  descr = "Display the legend" ;
  doc = "By default, will display it if there are more than one plot." ;
  setter = fun s ->
    let g = get_current_graph no_renew in
    g.draw_legend_was_set <- true ;
    g.draw_legend <- legend_location_of_string s
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

and load_file_config confdir file =
  let args = params_of_file_config confdir file in
  parse_options [ file_options ] args

and file_options = [| {
  names = [| "separator" |] ;
  has_param = true ;
  descr = "character to use as field separator" ;
  doc = "" ;
  setter = (fun s ->
    if String.length s != 1 then
      invalid_arg "separator must be a single character" ;
    (get_current_file ()).separator <- s.[0]) ;
} ; {
  names = [| "has-header" ; "header" |] ;
  has_param = false ;
  descr = "Does the first line of the CSV has labels" ;
  doc = "" ;
  setter = fun s ->
    (get_current_file ()).has_header <- bool_of_string s
} ; {
  names = [| |] ;
  has_param = false ;
  descr = "CSV file" ;
  doc = "" ;
  setter = fun s ->
    (try (* Check this is a file *)
      let ic = Unix.(openfile s [ O_RDONLY ] 0o644) in
      Unix.close ic
    with _ -> raise Not_found) ;
    let g = get_current_graph no_renew in
    let f = make_new_file s in
    load_file_config global.confdir f ; (* may not find anything if we use a confname ; we will try again later *)
    g.files <- append g.files f
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
  setter = fun s ->
    let file = get_current_file () in
    file.confname <- s ;
    (* Try again to load the conig (overwriting the cli params that have been set between file name and  --confname) *)
    load_file_config global.confdir file
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

let rec load_field_config confdir file index =
  let args = params_of_field_config confdir file index in
  parse_options [ field_options ] args

and field_options = [| {
  names = [| "x" |] ;
  has_param = true ;
  descr = "field number (starting at 0) of the X value for this graph" ;
  doc = "" ;
  setter = fun s ->
    let f = get_current_x_field () in
    if f.index <> ~-1 then
      raise (ParseError "Set twice the X field") ;
    f.index <- (int_of_string s - 1) ; (* TODO: index of string that validates >= 0 *)
    load_field_config global.confdir (get_current_file ()) f.index
} ; {
  names = [| "y" ; "y1" |] ;
  has_param = true ;
  descr = "field number of the next value reported on the left Y axis" ;
  doc = "" ;
  setter = fun s ->
    let idx = int_of_string s - 1 in
    let file = get_current_file () in
    file.y1_fields <- new_field file.y1_fields idx ;
    load_field_config global.confdir (get_current_file ()) idx
} ; {
  names = [| "y2" |] ;
  has_param = true ;
  descr = "field number of the next value reported on the right Y axis" ;
  doc = "" ;
  setter = fun s ->
    let idx = int_of_string s - 1 in
    let file = get_current_file () in
    file.y2_fields <- new_field file.y2_fields idx ;
    load_field_config global.confdir (get_current_file ()) idx
} ; {
  names = [| "annot" ; "y3" |] ;
  has_param = true ;
  descr = "field number of the next value to use as annotation" ;
  doc = "" ;
  setter = (fun s ->
    let idx = int_of_string s - 1 in
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
  setter = fun s ->
    let field = get_last_field () in
    field.pen.color_was_set <- true ;
    field.pen.color <- Color.of_string s
} ; {
  names = [| "opacity" |] ;
  has_param = true ;
  descr = "opacity" ;
  doc = "" ;
  setter = fun s ->
    (get_last_field ()).pen.opacity <- float_of_string s
} ; {
  names = [| "fill-opacity" |] ;
  has_param = true ;
  descr = "fill opacity" ;
  doc = "" ;
  setter = fun s ->
    (get_last_field ()).pen.fill_opacity <- float_of_string s
} ; {
  names = [| "stroke-width" |] ;
  has_param = true ;
  descr = "stroke width" ;
  doc = "" ;
  setter = fun s ->
    (get_last_field ()).pen.stroke_width <- float_of_string s
} ; {
  names = [| "filled" ; "fill" |] ;
  has_param = false ;
  descr = "should the area below this value be filled?" ;
  doc = "" ;
  setter = fun s ->
    (get_last_field ()).pen.filled <- bool_of_string s
} ; {
  names = [| "format" |] ;
  has_param = true ;
  descr = "numeric|timestamp|date(...a la strptime...)" ;
  doc = "" ;
  setter = fun s ->
    let len = String.length in
    let f = get_last_field () in
    f.fmt <- List.find_map (fun (fmtname, f) ->
      if s = fmtname then Some (f "") else
      if String.starts_with s fmtname &&
         s.[len fmtname] = '(' &&
         s.[len s - 1] = ')' then
        Some (f (String.sub s (len fmtname + 1)
                              (len s - len fmtname - 2)))
      else None) Formats.all ;
    f.fmt_was_set <- true
} |]

(* Other options are just for this run and not backed by any config file *)

let open_browser = ref true
let print_help = ref false
let output_svg = ref false

let other_options = [| {
  names = [| "open-browser" ; "open" |] ;
  has_param = false ;
  descr = "automatically launch the browser" ;
  doc = "You may need to use the --opan-browser-with option to configure\
         the details." ;
  setter = fun v -> open_browser := bool_of_string v ;
} ; {
  names = [| "help" |] ;
  has_param = false ;
  descr = "print this help" ;
  doc = "" ;
  setter = fun v -> print_help := bool_of_string v ;
} ; {
  names = [| "output-svg" ; "svg-only" ; "svg" |] ;
  has_param = false ;
  descr = "dump the SVG and exit" ;
  doc = "" ;
  setter = fun s -> output_svg := bool_of_string s
} |]

(*
 * Command Line
 *)

let iter_fields file f =
  f 0 file.x_field ;
  Array.iter (f 1) file.y1_fields ;
  Array.iter (f 2) file.y2_fields ;
  Array.iter (f 3) file.annot_fields

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
      exit 1) args ;
  (* Arrange configuration *)
  Array.iter (fun g ->
    let nb_fields = Array.make 4 0 in
    Array.iter (fun file ->
        (* First: get the labels from the header: *)
        if file.has_header then (
          Printf.eprintf "read labels...\n%!" ;
          let fd = Unix.(openfile file.fname [O_RDONLY; O_CLOEXEC] 0o644) in
          let str = Read_csv.read_at fd 0 file.block_size in
          Unix.close fd ;
          file.data_start <- String.index str '\n' + 1 ;
          let labels = String.sub str 0 (file.data_start - 1) |>
                       String.split_on_char file.separator |>
                       Array.of_list in
          (* set the label for all fields with default label *)
          iter_fields file (fun _axis field ->
            if field.label == default_label then
              field.label <- labels.(field.index))
        ) ;
        iter_fields file (fun axis field ->
          if not field.pen.color_was_set then
            field.pen.color <- Color.random_of_string field.label ;
          nb_fields.(axis) <- nb_fields.(axis) + 1)
      ) g.files ;
    if nb_fields.(0) != 1 then (
      Printf.eprintf "You must have 1 X field.\n" ;
      exit 1) ;
    if nb_fields.(1) + nb_fields.(2) < 1 then (
      Printf.eprintf "You must have at least 1 Y field.\n" ;
      exit 1) ;
    if not g.draw_legend_was_set then
      g.draw_legend <-
        if nb_fields.(1) + nb_fields.(2) > 1 then UpperRight else NoShow ;
    ) !graphs ;
  (* Save configuration *)
  if global.save_config then (
    Printf.eprintf "Saving the configuration\n" ;
    save_global_config global.confdir global ;
    Array.iter (fun g ->
      save_graph_config global.confdir g) !graphs
  )

(* Refresh a config file info *)
let update_file_info f =
  Printf.eprintf "  Check file %s...\n" f.fname ;
  f.fd <- Unix.(openfile f.fname [O_RDONLY; O_CLOEXEC] 0o644) ;
  let sz = Read_csv.file_size f.fd in
  if sz <> f.size then (
    f.size <- sz ;
    Printf.eprintf "    size is now %d\n" sz ;
    (try
      f.last_x <-
        Read_csv.get_last_x f.fd f.size f.block_size f.separator f.x_field.index f.x_field.fmt.Formats.to_value ;
      Printf.eprintf "    last x is now %f\n" f.last_x
     with Not_found -> ()) ;
    if f.first_x = 0. then
      (try
        f.first_x <-
          Read_csv.get_first_x f.fd f.data_start f.size f.block_size f.separator f.x_field.index f.x_field.fmt.Formats.to_value ;
        Printf.eprintf "    first x is now %f\n" f.first_x
       with Not_found -> ()) ;
  )
