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
  mutable width : float ;
  mutable filled : bool ;
  mutable opacity : float ;
}

type file = {
  mutable fname : string ;
  mutable separator : char ;
  mutable x_field : field ;
  mutable y1_fields : field array ;
  mutable y2_fields : field array ;
  mutable annot_fields : field array ;
  mutable fd : Unix.file_descr ;
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
}

type global = {
  mutable confdir : string ;
  mutable open_browser_with : string ;
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
  width = 1. ; filled = false ; opacity = 1.
}

let make_new_file fname =
  last_field := None ;
  {
    fname ; separator = ',' ;
    x_field = make_new_field ~-1 ;
    y1_fields = [| |] ;
    y2_fields = [| |] ;
    annot_fields = [| |] ;
    fd = Unix.stdin ; size = -1 ;
    first_x = 0. ; last_x = 0. ;
    block_size = 4096 ;
  }

let make_new_graph () = {
  title = default_title ;
  files = [| |] ;
  x_label = default_x_label ;
  y1_label = default_y1_label ;
  y2_label = default_y2_label ;
  y1_stacked = false ; y2_stacked = false ;
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
  g.files <- append g.files f ;
  f

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
  setter = (fun s ->
    let f = new_file s in
    f.fd <- Unix.(openfile s [O_RDONLY; O_CLOEXEC] 0o644)) ;
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

(* FIXME: no need for more than one option list *)
let field_options = [| {
  names = [| "x" |] ;
  has_param = true ;
  descr = "field number (starting at 0) of the X value for this graph" ;
  doc = "" ;
  setter = (fun s ->
    let f = get_current_x_field () in
    if f.index <> ~-1 then
      raise (ParseError "Set twice the X field") ;
    f.index <- int_of_string s) ;
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
  names = [| "width" |] ;
  has_param = true ;
  descr = "stroke width" ;
  doc = "" ;
  setter = (fun s -> (get_last_field ()).width <- float_of_string s)
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

(* Parse *)

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

let try_parse_option opts n v =
  match chop_dashes n with
  | exception Not_found ->
    false
  | n ->
    if Array.exists (fun opt ->
        Array.exists ((=) n) opt.names &&
        (match opt.has_param, v with
        | true, Some v ->
          opt.setter v ; true
        | false, _ ->
          opt.setter "true" ; true
        | _ -> false) ||
        not opt.has_param &&
        String.starts_with n "no" &&
        Array.exists ((=) (chop_dashes (chop n 2))) opt.names &&
        (opt.setter "false" ; true)
      ) opts then true
    else invalid_arg n

let try_parse_bareword opts v =
  Array.exists (fun opt ->
      Array.length opt.names = 0 &&
      (opt.setter v ; true)
    ) opts

let parse_args args =
  let options = [
    "Runtime options", other_options ;
    "Global options", global_options ;
    "Graph options", graph_options ;
    "File options", file_options ;
    "Field options", field_options ] in
  let rec loop test_opts i =
    if i < Array.length args then (
      let p = args.(i) in
      if p = "--" then loop false (i+1) else (
        let v =
          if i < Array.length args - 1 then Some args.(i+1)
          else None in
        if not (test_opts && List.exists (fun (_, opts) ->
          try_parse_option opts p v) options) then
        if not (List.exists (fun (_, opts) ->
          try_parse_bareword opts p) options) then
        Printf.eprintf "What do you mean by '%s'?\n" p
      )
    ) in
  loop true 1 ;  (* TODO: get exceptions *)
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
      Printf.printf "\n") options ;
    exit 0
  )

