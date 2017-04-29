open Batteries

let debug = false

exception ParseError of string

let default_title = "title"
let default_y_label = "Y"
let default_label = "label"

(* Config file: super hackish! Should be rewritten, with each parameter (above)
 * having a getter that either take a value set by command line or fallback to
 * the configuration file(s). *)

let mkdir_all ?(is_file=false) dir =
  if debug then Printf.eprintf "mkdir -p %s\n" dir ;
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
  if debug then Printf.eprintf "Storing config in file '%s'\n" fname ;
  let mode = [ `create; `trunc; `text ] in
  BatFile.with_file_out ~mode fname

module Pen = struct
  type t = {
    mutable color : Color.t ;
    mutable color_was_set : bool ;
    mutable opacity : float ;
    mutable stroke_width : float ;
    mutable filled : bool ;
    mutable fill_opacity : float ;
    mutable dasharray : string option ;
    mutable draw_line : bool ;
    mutable draw_points : bool ;
    mutable label : string ;
  }
  let current : t option ref = ref None
  let make_new ?(label=default_label) () =
    let t = {
      color = [| 0. ; 0. ; 0. |] ;
      color_was_set = false ;
      opacity = 1. ;
      stroke_width = 1. ;
      dasharray = None ;
      filled = false ;
      fill_opacity = 0.7 ;
      draw_line = true ;
      draw_points = false ;
      label ;
    } in
    current := Some t ;
    t

  let save_config oc pen =
      Printf.fprintf oc "%s%s%s\
                         --color\n%s\n\
                         --opacity\n%f\n\
                         --label\n%s\n\
                         --fill-opacity\n%f\n"
                (if pen.filled then "--filled\n" else "")
                (if pen.draw_line then "--line\n" else "")
                (if pen.draw_points then "--points\n" else "")
                (Color.to_string pen.color)
                pen.opacity
                pen.label
                pen.fill_opacity
end

module Field = struct
  type t = {
    mutable index : int ;
    mutable fmt : Formats.t ;
    mutable fmt_was_set : bool ; (* track those that were explicitly set *)
    mutable pen : Pen.t ;
    mutable linear_regression : bool ;
  }
  let current : t option ref = ref None

  (* We create a new graph when we set again a field that has already been set.
   * But for stacked which cannot be compared by address. *)
  let make_new index =
    let t = {
      index ;
      fmt = Formats.numeric "" ;
      fmt_was_set = false ;
      pen = Pen.make_new () ;
      linear_regression = false ;
    } in
    current := Some t ;
    t

  let save_config confdir f =
    let fname = string_of_int f.index in
    with_save_file confdir fname (fun oc ->
      Printf.fprintf oc "--format\n%s\n"
        f.fmt.Formats.name ;
      if f.linear_regression then
        Printf.fprintf oc "--linear-regression\n" ;
      Pen.save_config oc f.pen)
end

type bounds = {
  mutable first_x : float ;
  mutable last_x : float ;
}

module File = struct
  type t = {
    fname : string ;
    mutable confname : string ;
    mutable fd : Unix.file_descr ;
    mutable separator : char ;
    mutable has_header : bool ;
    mutable x_field : Field.t option ;
    mutable y1_fields : Field.t array ;
    mutable y2_fields : Field.t array ;
    mutable annot_fields : Field.t array ;
    mutable data_start : int ;  (* not 0 if has_header *)
    mutable size : int ;
    mutable block_size : int ; (* a size that worked so far *)
    bounds : bounds
  }
  let make_new fname =
    Field.current := None ;
    Pen.current := None ;
    { fname ; confname = "" ;
      separator = ',' ;
      has_header = false ;
      fd = Unix.stdin ; (* typechecks *)
      x_field = None ;
      y1_fields = [| |] ;
      y2_fields = [| |] ;
      annot_fields = [| |] ;
      data_start = 0; size = -1 ;
      block_size = 1024 ;
      bounds = { first_x = 0. ; last_x = 0. } }

  let confname f =
    (if f.confname <> "" then f.confname else f.fname) |>
    Filename.basename

  let save_config confdir file =
    let confdir = confdir ^"/files" in
    let fname = confname file in
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
    Option.may (Field.save_config file_confdir) file.x_field ;
    Array.iter (Field.save_config file_confdir) file.y1_fields ;
    Array.iter (Field.save_config file_confdir) file.y2_fields

  (* Refresh a config file info *)
  let update_info f =
    if debug then Printf.eprintf "  Check file %s...\n" f.fname ;
    let x_field = Option.get f.x_field in
    assert (x_field.Field.index >= 0) ;
    f.fd <- Unix.(openfile f.fname [O_RDONLY; O_CLOEXEC] 0o644) ;
    let sz = Read_csv.file_size f.fd in
    if sz <> f.size then (
      f.size <- sz ;
      if debug then Printf.eprintf "    size is now %d\n" sz ;
      (try
        f.bounds.last_x <-
          Read_csv.get_last_x f.fd f.size f.block_size f.separator x_field.Field.index x_field.Field.fmt.Formats.to_value ;
        if debug then Printf.eprintf "    last x is now %f\n" f.bounds.last_x
       with Not_found -> ()) ;
      if f.bounds.first_x = 0. then
        (try
          f.bounds.first_x <-
            Read_csv.get_first_x f.fd f.data_start f.size f.block_size f.separator x_field.Field.index x_field.Field.fmt.Formats.to_value ;
          if debug then Printf.eprintf "    first x is now %f\n" f.bounds.first_x
         with Not_found -> ()) ;
    )
end

(* instead of a file one can also enter an expression: *)
module Expr = struct
  type t = {
    expression : string ;
    funct : float -> float ;
    mutable pen : Pen.t ;
    mutable on_y1_axis : bool ;
    mutable bounds : bounds
    (* TODO: we may want to set a validity domain *)
  }

  let make_new expression alg_val = {
    expression ;
    funct = (let open Algebra in
      match alg_val with
      | Imm v -> fun _ -> v
      | Fun f -> f) ;
    on_y1_axis = true ;
    pen = Pen.make_new ~label:expression () ;
    bounds = { first_x = 0. ; last_x = 0. } ;
  }
end

type data_source =
  | File of File.t
  | Expr of Expr.t

module Source = struct
  type t = data_source

  let bounds = function
    | File file -> file.File.bounds
    | Expr expr -> expr.Expr.bounds
end

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

module Graph = struct
  type t = {
    mutable title : string ;
    mutable files : data_source array ;
    mutable x_label : string ; (* or use files.(0).x_field.pen.label - allows to set either label at the field level or at the graph level. *)
    mutable y1_label : string ;
    mutable y2_label : string ;
    mutable y1_stacked : stacked ;
    mutable y2_stacked : stacked ;
    mutable x_start : float option ; (* initial starting position *)
    mutable x_stop : float option ;
    mutable x_tick_spacing : float option ;
    mutable y_tick_spacing : float option ;
    mutable force_show_0 : bool ;
    mutable font_size : float ;
    mutable draw_legend : legend_location ;
    mutable draw_legend_was_set : bool ;
    width : int option ;
    height : int option ;
  }

  let make_new () = {
    title = default_title ;
    files = [| |] ;
    x_label = "" ;
    y1_label = default_y_label ;
    y2_label = default_y_label ;
    y1_stacked = NotStacked ;
    y2_stacked = NotStacked ;
    x_start = None ; x_stop  = None ;
    force_show_0 = false ;
    x_tick_spacing = None ;
    y_tick_spacing = None ;
    font_size = 14. ;
    draw_legend = UpperRight ;
    draw_legend_was_set = false ;
    width = None ;
    height = None ;
  }

  let source_of_linreg g i =
    let s = if i > 0 then i-1 else 1 in
    if s > Array.length g.files then
      raise (ParseError "Cannot display linear regression of nothing.") ;
    s
end

type global = {
  mutable save_config : bool ;
  mutable open_browser_with : string ;
  mutable default_width : int ;
  mutable default_height : int ;
}

let params_of_file confdir name =
  let fname = confdir ^"/"^ name in
  BatFile.lines_of fname |>
  Enum.map Option.some |>
  Array.of_enum

let params_of_field_config confdir file n =
  let confdir = confdir ^"/files/"^ File.confname file ^".fields" in
  try params_of_file confdir (string_of_int n)
  with exn ->
    Printf.eprintf "Cannot read field %d configuration from %s: %s\n"
      n confdir (Printexc.to_string exn) ;
    [| |]

let params_of_file_config confdir file =
  let confdir = confdir ^"/files/"^ File.confname file in
  try params_of_file confdir (File.confname file)
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

let save_graph_config confdir g =
  with_save_file (confdir ^"/graphs") (to_fname g.Graph.title) (fun oc ->
    Printf.fprintf oc "--y1-label\n%s\n\
                       --y2-label\n%s\n\
                       --x-label\n%s\n\
                       %s\n%s\n\
                       --font-size\n%f\n\
                       --legend\n%s\n"
      g.Graph.y1_label g.Graph.y2_label g.Graph.x_label
      (string_of_stacked "y1" g.Graph.y1_stacked)
      (string_of_stacked "y2" g.Graph.y2_stacked)
      g.Graph.font_size
      (string_of_legend g.Graph.draw_legend) ;
    print_opt_float oc "--x-start" g.Graph.x_start ;
    print_opt_float oc "--x-stop" g.Graph.x_stop ;
    if g.Graph.force_show_0 then Printf.fprintf oc "--force-show-0\n" ;
    Option.may (fun v -> Printf.fprintf oc "--x-tick-spacing\n%f\n" v)
      g.Graph.x_tick_spacing ;
    Option.may (fun v -> Printf.fprintf oc "--y-tick-spacing\n%f\n" v)
      g.Graph.y_tick_spacing ;
    print_opt_int oc "--width" g.Graph.width ;
    print_opt_int oc "--height" g.Graph.height ;
    Array.iter (function
        | File file ->
          Printf.fprintf oc "%s\n" file.File.fname ;
          if file.File.confname <> "" then
            Printf.fprintf oc "--confname\n%s\n" file.File.confname ;
          File.save_config confdir file
        | Expr expr ->
          Printf.fprintf oc "%s\n" expr.Expr.expression
          (* TODO: Expr.save_config *)
      ) g.Graph.files)

let params_of_graph_config confdir g =
  let confdir = confdir ^"/graphs" in
  try params_of_file confdir (to_fname g.Graph.title)
  with exn ->
    Printf.eprintf "Cannot read graph configuration from %s: %s\n"
      confdir (Printexc.to_string exn) ;
    [| |]


type cli_option = {
  names : string array ; (* First one will be the one in the ini file *)
  has_param : bool ;
  descr : string ;
  doc : string ;
  setter : string -> unit ;
}

(* Other options are just for this run and not backed by any config file *)

let confdir =
    let homedir =
      try Unix.getenv("HOME")
      with Not_found -> "/tmp" in
    ref (homedir ^"/.csview")
let open_browser = ref true
let print_help = ref false
let output_svg = ref false

let other_options = [| {
  names = [| "confdir" |] ;
  has_param = true ;
  descr = "directory where configuration files are stored" ;
  doc = "Where to read and optionally write settings." ;
  setter = (fun s -> confdir := s) ;
} ; {
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



let global = {
  save_config = false ;
  open_browser_with = CompilConfig.default_open ^" http://localhost:%port%" ;
  default_width = 800 ;
  default_height = 600 ;
}

let global_options = [| {
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
  setter = fun s -> global.open_browser_with <- s
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
  loop 0

let save_global_config confdir global =
  with_save_file confdir "global" (fun oc ->
    Printf.fprintf oc "--open-browser-with\n%s\n\
                       --default-width\n%d\n\
                       --default-height\n%d\n"
      global.open_browser_with
      global.default_width
      global.default_height)

let load_global_config confdir =
  try params_of_file confdir "global" |>
      parse_options [ global_options ]
  with exn ->
    Printf.eprintf "Cannot read global configuration from %s: %s\n"
      confdir (Printexc.to_string exn)

let graphs = ref [| |]

let last_entry a = a.( Array.length a - 1 )

let append a x =
  let len = Array.length a + 1 in
  Array.init len (fun i ->
    if i = len - 1 then x else a.(i))

let get_current_graph renew =
  if Array.length !graphs = 0 ||
     renew (last_entry !graphs) then (
    let new_graph = Graph.make_new () in
    graphs := append !graphs new_graph ;
    new_graph
  ) else last_entry !graphs

let no_renew _ = false

(* For files we start a new one after each bareword parameter (file name) *)
let get_current_file () =
  let g = get_current_graph no_renew in
  let file_of_source = function
    | File f -> f
    | Expr _ ->
      raise (ParseError "Option does not apply to algebraic expression") in
  match last_entry g.Graph.files with
    | exception Invalid_argument _ ->
      raise (ParseError "Must give the file name before the file options")
    | src -> file_of_source src

let get_current_pen () =
  match !Pen.current with
  | None ->
    raise (ParseError "Cannot configure a pen before specifying a field")
  | Some p -> p

let rec load_graph_config confdir g =
  let args = params_of_graph_config confdir g in
  parse_options [ graph_options ; file_options ] args

and graph_options = [| {
  names = [| "title" |] ;
  has_param = true ;
  descr = "title of that graph" ;
  doc = "" ;
  setter = fun s ->
    let renew g = g.Graph.title != default_title in
    let g = get_current_graph renew in
    g.Graph.title <- s ;
    load_graph_config !confdir g
} ; {
  names = [| "y-label" ; "y1-label" |] ;
  has_param = true ;
  descr = "Y axis label" ;
  doc = "" ;
  setter = (fun s ->
    let renew g = g.Graph.y1_label != default_y_label in
    (get_current_graph renew).Graph.y1_label <- s) ;
} ; {
  names = [| "y2-label" |] ;
  has_param = true ;
  descr = "right-Y axis label" ;
  doc = "" ;
  setter = (fun s ->
    let renew g = g.Graph.y2_label != default_y_label in
    (get_current_graph renew).Graph.y2_label <- s) ;
} ; {
  names = [| "x-label" |] ;
  has_param = true ;
  descr = "Same as label of the x axis" ;
  doc = "" ;
  setter = (fun s ->
    let renew g = g.Graph.x_label <> "" in
    (get_current_graph renew).Graph.x_label <- s) ;
} ; {
  names = [| "stacked" ; "y1-stacked" |] ;
  has_param = false ;
  descr = "Should values be stacked" ;
  doc = "This is only for values plotted against the left Y axis." ;
  setter = (fun s ->
    (get_current_graph no_renew).Graph.y1_stacked <-
      if bool_of_string s then Stacked else NotStacked) ;
} ; {
  names = [| "stackcentered" ; "stackedcentered" ; "stackedcenter" ;
             "y1-stackcentered" ; "y1-stackedcentered" ;
             "y1-stackedcenter" |] ;
  has_param = false ;
  descr = "Should values be stacked (smarter stacking)" ;
  doc = "This is only for values plotted against the left Y axis." ;
  setter = (fun s ->
    (get_current_graph no_renew).Graph.y1_stacked <-
      if bool_of_string s then StackedCentered else NotStacked) ;
} ; {
  names = [| "y2-stacked" |] ;
  has_param = false ;
  descr = "Should right values be stacked" ;
  doc = "This is only for values plotted against the right Y axis." ;
  setter = (fun s ->
    (get_current_graph no_renew).Graph.y2_stacked <-
      if bool_of_string s then Stacked else NotStacked) ;
} ; {
  names = [| "stackedcentered" ; "stackedcenter" ; "y1-stackcentered" ;
             "y1-stackedcentered" ; "y1-stackedcenter" |] ;
  has_param = false ;
  descr = "Should right values be stacked (smarter stacking)" ;
  doc = "This is only for values plotted against the right Y axis." ;
  setter = (fun s ->
    (get_current_graph no_renew).Graph.y2_stacked <-
      if bool_of_string s then StackedCentered else NotStacked) ;
} ; {
  names = [| "force-show-0" ; "force-0" ; "show-0" |] ;
  has_param = false ;
  descr = "Force the Y axis to include 0" ;
  doc = "" ;
  setter = (fun s ->
    (get_current_graph no_renew).Graph.force_show_0 <- s = "true") ;
} ; {
  names = [| "x-tick-spacing" |] ;
  has_param = true;
  descr = "Approximate distance between successive ticks on the X axis" ;
  doc = "" ;
  setter = fun s ->
    (get_current_graph no_renew).Graph.x_tick_spacing <- Some (float_of_string s)
} ; {
  names = [| "y-tick-spacing" |] ;
  has_param = true;
  descr = "Approximate distance between successive ticks on the Y axis" ;
  doc = "" ;
  setter = fun s ->
    (get_current_graph no_renew).Graph.y_tick_spacing <- Some (float_of_string s)
} ; {
  names = [| "font-size" |] ;
  has_param = true ;
  descr = "font size" ;
  doc = "" ;
  setter = fun s ->
    (get_current_graph no_renew).Graph.font_size <- float_of_string s
} ; {
  names = [| "legend" ; "show-legend" ; "legend-position" ;
             "legend-location" |] ;
  has_param = true ;
  descr = "Display the legend" ;
  doc = "By default, will display it if there are more than one plot." ;
  setter = fun s ->
    let g = get_current_graph no_renew in
    g.Graph.draw_legend_was_set <- true ;
    g.Graph.draw_legend <- legend_location_of_string s
} ; {
  names = [| "start-x" ; "x-start" ; "start" |] ;
  has_param = true ;
  descr = "Initial starting value for the X axis" ;
  doc = "First value of first file if unset." ;
  setter = (fun s ->
    (get_current_graph no_renew).Graph.x_start <- Some (float_of_string s)) ;
} ; {
  names = [| "stop-x" ; "x-stop" ; "stop" |] ;
  has_param = true ;
  descr = "Initial final value for the X axis" ;
  doc = "Last value of first file is unset." ;
  setter = (fun s ->
    (get_current_graph no_renew).Graph.x_stop <- Some (float_of_string s)) ;
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
    (get_current_file ()).File.separator <- s.[0]) ;
} ; {
  names = [| "has-header" ; "header" |] ;
  has_param = false ;
  descr = "Does the first line of the CSV has labels" ;
  doc = "" ;
  setter = fun s ->
    (get_current_file ()).File.has_header <- bool_of_string s
} ; {
  names = [| |] ;
  has_param = false ;
  descr = "CSV file or algebraic expression" ;
  doc = "" ;
  setter = fun s ->
    if debug then Printf.eprintf "Adding file %s\n" s ;
    let f_or_e =
      try (* Check this is a file *)
        let ic = Unix.(openfile s [ O_RDONLY ] 0o644) in
        Unix.close ic ;
        let f = File.make_new s in
        (* may not find anything if we use a confname ; we will try again later *)
        load_file_config !confdir f ;
        if debug then Printf.eprintf "...was a file, added\n" ;
        File f
      with Unix.Unix_error (Unix.ENOENT, _, _) ->
        (* Try an algebraic expression *)
        let open Algebra.P in
        let e = ParsersBoundedSet.make 0 in
        let st = Algebra.stream_of_string s in
        let p = Algebra.operation +- eof in
        (match p [] None e st |> to_result with
        | Ok (f, (_pos, [])) ->
          if debug then Printf.eprintf "...was an expression, added\n" ;
          Expr (Expr.make_new s f)
        | Ok _ -> assert false (* Cannot happen *)
        | Bad _ -> raise Not_found)
       | _ -> raise Not_found in
    let g = get_current_graph no_renew in
    g.Graph.files <- append g.Graph.files f_or_e ;
    if debug then Printf.eprintf "Now have %d files\n"
      (Array.length g.Graph.files)
} ; {
  names = [| "block-size" |] ;
  has_param = true ;
  descr = "any block of that size should contain at least a full line." ;
  doc = "" ;
  setter = (fun s ->
    (get_current_file ()).File.block_size <- int_of_string s) ;
} ; {
  names = [| "max-line-length" ; "max-line-len" |] ;
  has_param = true ;
  descr = "max line length" ;
  doc = "Including the newline character." ;
  setter = (fun s ->
    (get_current_file ()).File.block_size <- 2 * int_of_string s) ;
} ; {
  names = [| "confname" ; "conf-name" ; "config-name" |] ;
  has_param = true ;
  descr = "assume this file name for fetching/saving configuration" ;
  doc = "" ;
  setter = fun s ->
    let file = get_current_file () in
    file.File.confname <- s ;
    (* Try again to load the config (overwriting the cli params that have been set between file name and  --confname) *)
    load_file_config !confdir file
} |]

(* We create a new field each time we set a value that was already set *)

let get_current_field () =
  match !Field.current with
  | None ->
    raise (ParseError "Cannot configure a field before giving its index")
  | Some f -> f

(* TODO: pass also a range of possible values so that we know what
 * accuracy is required! *)
let string_of_timestamp ts =
  let open Unix in
  let tm = localtime (float_of_string ts) in
  Printf.sprintf "%04d-%02d-%02d %02d:%02d:%02d"
    (tm.tm_year + 1900) (tm.tm_mon + 1) tm.tm_mday
    tm.tm_hour tm.tm_min tm.tm_sec

let check_field_index idx =
  if idx < 0 then raise (ParseError "Field indexes starts at 1")

let rec load_field_config confdir file index =
  let args = params_of_field_config confdir file index in
  parse_options [ field_options ; pen_options ] args

and field_options = [| {
  names = [| "x" |] ;
  has_param = true ;
  descr = "field number (starting at 1) of the X value for this graph" ;
  doc = "" ;
  setter = fun s ->
    let idx = int_of_string s - 1 in
    check_field_index idx ;
    let file = get_current_file () in
    if file.File.x_field <> None then
      raise (ParseError "Set twice the X field") ;
    file.File.x_field <- Some (Field.make_new idx) ;
    load_field_config !confdir file idx
} ; {
  names = [| "y" ; "y1" |] ;
  has_param = true ;
  descr = "field number of the next value reported on the left Y axis" ;
  doc = "" ;
  setter = fun s ->
    let idx = int_of_string s - 1 in
    check_field_index idx ;
    let file = get_current_file () in
    file.File.y1_fields <- append file.File.y1_fields (Field.make_new idx) ;
    load_field_config !confdir file idx
} ; {
  names = [| "y2" |] ;
  has_param = true ;
  descr = "field number of the next value reported on the right Y axis" ;
  doc = "" ;
  setter = fun s ->
    let idx = int_of_string s - 1 in
    check_field_index idx ;
    let file = get_current_file () in
    file.File.y2_fields <- append file.File.y2_fields (Field.make_new idx) ;
    load_field_config !confdir file idx
} ; {
  names = [| "annot" ; "y3" |] ;
  has_param = true ;
  descr = "field number of the next value to use as annotation" ;
  doc = "" ;
  setter = fun s ->
    let idx = int_of_string s - 1 in
    check_field_index idx ;
    let file = get_current_file () in
    file.File.annot_fields <- append file.File.annot_fields (Field.make_new idx)
} ; {
  names = [| "format" |] ;
  has_param = true ;
  descr = "numeric|timestamp|date(...a la strptime...)" ;
  doc = "" ;
  setter = fun s ->
    let len = String.length in
    let f = get_current_field () in
    f.Field.fmt <- List.find_map (fun (fmtname, f) ->
      if s = fmtname then Some (f "") else
      if String.starts_with s fmtname &&
         s.[len fmtname] = '(' &&
         s.[len s - 1] = ')' then
        Some (f (String.sub s (len fmtname + 1)
                              (len s - len fmtname - 2)))
      else None) Formats.all ;
    f.Field.fmt_was_set <- true
} ; {
  names = [| "linear-regression" ; "linear-reg" ; "lin-reg" ; "linreg" |] ;
  has_param = false ;
  descr = "display a linear regression for this field" ;
  doc = "" ;
  setter = fun s ->
    (get_current_field ()).Field.linear_regression <- bool_of_string s
} |]

and pen_options = [| {
  names = [| "label" |] ;
  has_param = true ;
  descr = "label for this field" ;
  doc = "" ;
  setter = (fun s -> (get_current_pen ()).Pen.label <- s)
} ; {
  names = [| "color" ; "col" |] ;
  has_param = true ;
  descr = "color to use for this field" ;
  doc = "" ;
  setter = fun s ->
    let pen = get_current_pen () in
    pen.Pen.color_was_set <- true ;
    pen.Pen.color <- Color.of_string s
} ; {
  names = [| "opacity" |] ;
  has_param = true ;
  descr = "opacity" ;
  doc = "" ;
  setter = fun s ->
    (get_current_pen ()).Pen.opacity <- float_of_string s
} ; {
  names = [| "fill-opacity" |] ;
  has_param = true ;
  descr = "fill opacity" ;
  doc = "" ;
  setter = fun s ->
    (get_current_pen ()).Pen.fill_opacity <- float_of_string s
} ; {
  names = [| "stroke-width" |] ;
  has_param = true ;
  descr = "stroke width" ;
  doc = "" ;
  setter = fun s ->
    (get_current_pen ()).Pen.stroke_width <- float_of_string s
} ; {
  names = [| "filled" ; "fill" |] ;
  has_param = false ;
  descr = "should the area below this value be filled?" ;
  doc = "" ;
  setter = fun s ->
    (get_current_pen ()).Pen.filled <- bool_of_string s
} ; {
  names = [| "lines" ; "line" ; "draw-lines" ; "draw-line" |] ;
  has_param = false ;
  descr = "draw this field as a line" ;
  doc = "" ;
  setter = fun s ->
    (get_current_pen ()).Pen.draw_line <- bool_of_string s
} ; {
  names = [| "points" ; "point" ; "draw-points" ; "draw-point" |] ;
  has_param = false ;
  descr = "draw this field as points" ;
  doc = "" ;
  setter = fun s ->
    (get_current_pen ()).Pen.draw_points <- bool_of_string s
} ; {
  names = [| "dashed" |] ;
  has_param = false ;
  descr = "draw this field as a dashed line" ;
  doc = "" ;
  setter = fun s ->
    (get_current_pen ()).Pen.dasharray <- if bool_of_string s then Some "5,5" else None
} ; {
  names = [| "dash-array" ; "dash-pattern" ; "dasharray" ; "dashpattern" |] ;
  has_param = true ;
  descr = "draw this field as a dashed line, and set the dash pattern" ;
  doc = "The dash pattern is a list of integers, giving the length of \
         successive dashes and gaps (as in SVG attribute stroke-dasharray), \
         for instance \"5,10,5\"." ;
  setter = fun s ->
    (get_current_pen ()).Pen.dasharray <- Some s
} |]

(*
 * Command Line
 *)

let iter_fields file f =
  Option.may (f 0) file.File.x_field ;
  Array.iter (f 1) file.File.y1_fields ;
  Array.iter (f 2) file.File.y2_fields ;
  Array.iter (f 3) file.File.annot_fields

let parse_args args =
  (* We want the global and runtime options to be parsed first, and be
   * insensitive to their position in the command line, and then the other
   * options which effect depend on position. *)
  (* So we can remove options that are processed already: *)
  let args = Array.init (Array.length args - 1) (fun i -> Some args.(i+1)) in
  parse_options [ other_options ] args ;
  load_global_config !confdir ; (* need confdir from other_options *)
  parse_options [ global_options ] args ;
  parse_options [ graph_options ; file_options ; field_options ; pen_options ] args ;
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
        "Field options", field_options ;
        "Pen options", pen_options ] ;
    exit 0
  ) ;
  Array.iter (function
    | None -> ()
    | Some arg ->
      Printf.eprintf "Cannot parse '%s'\n" arg ;
      exit 1) args ;
  (* Arrange configuration *)
  if debug then Printf.eprintf "Arranging configuration of %d graphs...\n"
    (Array.length !graphs) ;
  Array.iteri (fun graph_idx g ->
    if debug then Printf.eprintf "Graph %d has %d files.\n"
      graph_idx (Array.length g.Graph.files) ;
    let nb_fields = Array.make 4 0 in
    Array.iter (function
        | File f ->
          (* If some fields are stacked and their pen have not been configured,
           * assign then some fill opacity? *)
          if debug then Printf.eprintf "Looking at file %s...\n"
            f.File.fname ;
          (* Check we have set x field *)
          if f.File.x_field = None then (* TODO: create it with index 0? *)
            raise (ParseError "Must set X field") ;
          (* Get the labels from the header: *)
          if f.File.has_header then (
            if debug then Printf.eprintf "read labels...\n%!" ;
            let fd = Unix.(openfile f.File.fname [O_RDONLY; O_CLOEXEC] 0o644) in
            (* File size is not yet known *)
            let sz = Read_csv.file_size fd in
            let str =
              Read_csv.read_at fd 0 (min sz f.File.block_size) in
            Unix.close fd ;
            f.File.data_start <- String.index str '\n' + 1 ;
            let labels = String.sub str 0 (f.File.data_start - 1) |>
                         String.split_on_char f.File.separator |>
                         Array.of_list in
            if debug then Printf.eprintf "labels = %a\n"
                            (Array.print String.print) labels ;
            (* set the label for all fields with default label *)
            iter_fields f (fun _axis field ->
              if field.Field.pen.Pen.label == default_label then
                field.Field.pen.Pen.label <- labels.(field.Field.index))
          ) ;
          iter_fields f (fun axis field ->
            if debug then Printf.eprintf "Considering field idx %d\n"
              field.Field.index ;
            if not field.Field.pen.Pen.color_was_set then
              field.Field.pen.Pen.color <-
                Color.random_of_string field.Field.pen.Pen.label ;
            nb_fields.(axis) <- nb_fields.(axis) + 1)
        | Expr expr ->
          if debug then Printf.eprintf "Looking at expression %s\n"
            expr.Expr.expression ;
            let axis = if expr.Expr.on_y1_axis then 1 else 2 in
            nb_fields.(axis) <- nb_fields.(axis) + 1
          (* TODO *)
      ) g.Graph.files ;
    if debug then Printf.eprintf "How many fields per axis: %a\n"
      (Array.print Int.print) nb_fields ;
    if nb_fields.(1) + nb_fields.(2) < 1 then (
      Printf.eprintf "You must have at least 1 Y field in graph %d.\n"
        graph_idx ;
      exit 1) ;
    if not g.Graph.draw_legend_was_set then
      g.Graph.draw_legend <-
        if nb_fields.(1) + nb_fields.(2) > 1 then UpperRight else NoShow ;
    ) !graphs ;
  (* Save configuration *)
  if global.save_config then (
    Printf.eprintf "Saving the configuration\n" ;
    save_global_config !confdir global ;
    Array.iter (fun g ->
      save_graph_config !confdir g) !graphs
  )

