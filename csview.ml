open Batteries
open Option.Infix

let debug = false

let port = 28019

(* Perf measurements *)
let times = Hashtbl.create 7
let with_timing n f =
  let start = Unix.gettimeofday () in
  let ret = f () in
  let stop = Unix.gettimeofday () in
  let dt = stop -. start in
  Hashtbl.modify_opt n (function
      | None -> Some (dt, 1)
      | Some (dt0, n) -> Some (dt0 +. dt, n+1)
    ) times ;
  ret

let () =
  Sys.(set_signal sigint (Signal_handle (fun _ -> exit 0))) ;
  at_exit (fun () ->
    if debug then Hashtbl.iter (fun k (dt, n) ->
          Printf.eprintf "Time spent %s: %fs (%fs x %d calls)\n"
            k dt (dt /. (float_of_int n)) n
        ) times)


(* We run in our own thread already thanks to [establish_server] so there
 * is no need for further ado.
 * Also, this is not the faster parser around but parsing an average HTTP
 * message takes about 5ms, which is much faster than what we are going to
 * do next... *)
module ParserConfig = ParsersConfig.BlockList (ParsersConfig.FileReader)
module ParserConfigWithOffset = ParsersPositions.Offset (ParserConfig)
let make_stream ic = ParserConfig.make_stream ic, 0
module HttpParser = CodecHttp.MakeParser (ParserConfigWithOffset)

let respond oc msg =
  Printf.fprintf oc "%s%!" (CodecHttp.Msg.encode msg)

let kaputt oc str =
  respond oc CodecHttp.(Msg.{
    start_line = StartLine.Response StatusLine.{
      version = 1, 1 ;
      code = 500 ;
      msg = "Kaputt" } ;
    headers = [
      "Content-Length", String.length str |> string_of_int ;
      "Content-Type", "text/plain" ] ;
    body = str })

let http_msg_of_html ?(content_type="text/html") html =
  let body = Printf.sprintf2 "%t%a" Html.print_xml_head Html.print html in
  CodecHttp.(Msg.{
    start_line = StartLine.Response StatusLine.{
      version = 1, 1 ;
      code = 200 ;
      msg = "Okay" } ;
    headers = [
      "Content-Length", String.length body |> string_of_int ;
      "Content-Type", content_type ;
    ] ;
    body })

let http_msg_of_svg svg =
  http_msg_of_html ~content_type:"image/svg+xml" svg

let read_whole_file fname =
  File.with_file_in fname IO.read_all

let content_type_of_file f =
  match String.rsplit f ~by:"." with
  | _, "html" -> "text/html"
  | _, "css"  -> "text/css"
  | _ -> "text/plain"

let http_msg_of_file fname =
  let body = read_whole_file fname in
  CodecHttp.(Msg.{
    start_line = StartLine.Response StatusLine.{
      version = 1, 1 ;
      code = 200 ;
      msg = "Okay" } ;
    headers = [
      "Content-Length", String.length body |> string_of_int ;
      "Content-Type", content_type_of_file fname ;
    ] ;
    body })

(* return a getter displaying the linear regression of the given getter, for n points *)
let linear_regression_getter getter n =
  let nf_inv = 1.0 /. float_of_int n in
  let rec loop i x_sum y_sum xy_sum x2_sum =
    if i >= n then (
      let b = (xy_sum -. x_sum *. y_sum *. nf_inv) /. (x2_sum -. x_sum *. x_sum *. nf_inv) in
      let a = (y_sum -. b *. x_sum) *. nf_inv in
      a, b
    ) else (
      let x = float_of_int i and y = getter i in
      loop (i+1) (x_sum +. x) (y_sum +. y) (xy_sum +. x *. y) (x2_sum +. x *. x)
    ) in
  let a, b = loop 0 0.0 0.0 0.0 0.0 in
  fun i -> a +. b *. float_of_int i

let get_svg g n t1 t2 =
  let open Config in
  let open File in
  let open Field in
  let fold_fields file init f =
    let prev = Array.fold_left (fun prev field ->
      f prev field true) init file.y1_fields in
    let prev = Array.fold_left (fun prev field ->
      f prev field false) prev file.y2_fields in
    (* TODO: annotations *)
    prev in
  let nf = float_of_int (n-1) in
  (* The fold below must iterate over all fields of all files of this graph.
   * data.(file_idx) = (pen, label, pri, getter) list ;
   * notice that we might have more "fields" here than we have in a file
   * because a given field may be redered using several ones (for linear
   * regressions, envelopes, stddevs...) *)
  let data =
    with_timing "reading data" (fun () ->
      Array.map (fun src -> match src with
          | File file ->
            let x_field = Option.get file.x_field in
            let lines = Read_csv.read_all file.fd x_field.index file.separator x_field.fmt.Formats.to_value file.block_size file.data_start file.size file.bounds.first_x file.bounds.last_x n t1 t2 in
            (* FIXME: instead of looping over field and then lines it would be faster to loop over lines and then fields,
             * thus we do not even need to store the lines *)
            fold_fields file [] (fun prev field pri ->
                let ys = Array.map (fun (_ts, line) ->
                  let y, _ = Read_csv.extract_field line file.separator 0 field.index field.fmt.Formats.to_value in
                  y) lines in
                let getter ts_idx = ys.(ts_idx) in
                let prev = (field.pen, pri, getter) :: prev in
                if field.linear_regression then
                  ({ field.pen with Pen.dasharray = Some "2,5" },
                   pri, linear_regression_getter getter n) :: prev
                else prev)
          | Expr expr ->
            let open Expr in
            let dt = t2 -. t1 in
            let getter ts_idx =
              let ts = t1 +.  dt *. (float_of_int ts_idx) /. nf in
              expr.funct ts in
            [ expr.pen, expr.on_y1_axis, getter ]
        ) g.Graph.files)
  in
  (* The fold function is supposed to accumulate over all datasets *)
  let fold = { Chart.fold = fun f init ->
    Array.fold_left (fun prev file_data ->
        List.fold_left (fun prev (pen, pri, getter) ->
            f prev pen pri getter
          ) prev file_data
      ) init data } in
  let vx_step = (t2-.t1) /. float_of_int (n-1) in
  (* For the labelling of X we take values and formatter from the first file.
   * But for the labelling of y1 and y2 we take the first defined value on
   * that axis: *)
  let string_of_y, string_of_y2 =
    let get_first_fmt fields =
      try
        let field = Array.find (fun field ->
          field.fmt_was_set) fields in
        Some field.fmt.Formats.to_label
      with Not_found -> None in
    Array.fold_left (fun (fmt1, fmt2 as prev) source ->
        match source with
        | File file ->
          let fmt1 = (match fmt1 with
            | None -> get_first_fmt file.y1_fields
            | _ -> fmt1)
          and fmt2 = (match fmt2 with
            | None -> get_first_fmt file.y2_fields
            | _ -> fmt2) in
          fmt1, fmt2
        | Expr _ -> prev
      ) (None, None) g.Graph.files in
    let file0 =
      Array.enum g.Graph.files |> Enum.find_map (function
        | File f -> Some f | Expr _ -> None) in
    let x_field0 = Option.get file0.x_field in
    let x_label =
      if g.Graph.x_label <> "" then g.Graph.x_label else x_field0.pen.Pen.label in
    with_timing "building SVG" (fun () ->
      Chart.xy_plot ~string_of_x:x_field0.fmt.Formats.to_label
                    ?string_of_y ?string_of_y2
                    ~svg_width:(float_of_int (g.Graph.width |? global.default_width))
                    ~svg_height:(float_of_int (g.Graph.height |? global.default_height))
                    ~axis_font_size:g.Graph.font_size
                    ~draw_legend:g.Graph.draw_legend
                    ~stacked_y1:g.Graph.y1_stacked
                    ~stacked_y2:g.Graph.y2_stacked
                    ~force_show_0:g.Graph.force_show_0
                    ~x_base:x_field0.fmt.Formats.base
                    ?y1_base:(try Some file0.y1_fields.(0).fmt.Formats.base
                              with Invalid_argument _ -> None)
                    ?y2_base:(try Some file0.y2_fields.(0).fmt.Formats.base
                              with Invalid_argument _ -> None)
                    ?x_tick_spacing:g.Graph.x_tick_spacing
                    ?y_tick_spacing:g.Graph.y_tick_spacing
                    x_label
                    g.Graph.y1_label
                    t1 vx_step n fold)

let get_graph oc params =
  let open Config in
  let get n = CodecUrl.get_single_query_param params n in
  let default v f = try f () with Not_found -> v in
  let gi = default 0 (fun () -> get "g" |> int_of_string) in
  let g = !graphs.(gi) in
  let bounds = Source.bounds g.Graph.files.(0) in
  let t1 = default bounds.first_x (fun () -> get "t1" |> float_of_string)
  and t2 = default bounds.last_x (fun () -> get "t2" |> float_of_string)
  and n = default 100 (fun () -> get "n" |> int_of_string) in
  let svg = get_svg g n t1 t2 in
  let msg = http_msg_of_svg svg in
  respond oc msg

let make_index_html _params =
  (* Can't be static because it depends on the number of graphs. *)
  let html_of_graph g i =
    let open Config in
    let attrs = [
      "width", string_of_int (g.Graph.width |? global.default_width) ;
      "height", string_of_int (g.Graph.height |? global.default_height) ] in
    let id = "graph_"^ string_of_int i in
    let bounds = Source.bounds g.Graph.files.(0) in
    let t1 = g.Graph.x_start |? bounds.first_x
    and t2 = g.Graph.x_stop  |? bounds.last_x in
    let url = "/graph.svg?g="^ string_of_int i ^
              "&t1="^ string_of_float t1 ^"&t2="^ string_of_float t2 in
    Html.Block (
      (if g.Graph.title <> "" then [ Html.h2 g.Graph.title ] else []) @
      [ Html.img ~attrs ~id url ]) in
  let html_of_graphs gs =
    let rec loop prev i =
      if i >= Array.length gs then List.rev prev
      else loop (html_of_graph gs.(i) i :: prev) (i+1) in
    loop [] 0 in
  Html.(html ~onload:"init()" [
    link_css "/style.css" ;
    tag "script" ~attrs:["src", "/csview.js"] [] ]
    (html_of_graphs !Config.graphs))

let on_all_http_msg oc msg =
  match msg.CodecHttp.Msg.start_line with
  | CodecHttp.StartLine.Request r ->
    let url =
      r.CodecHttp.RequestLine.url |>
      CodecUrl.of_string in
    if debug then Printf.printf "answering to %s...\n%!" (CodecUrl.to_string url) ;
    let params = CodecUrl.parse_query_of_url url in
    (match url.CodecUrl.path with
    | "/graph.svg" ->
      get_graph oc params
    | "/favicon.ico" | "/csview.js" | "/style.css" ->
      (* TODO: make www_root configurable *)
      http_msg_of_file (CompilConfig.default_www_root ^"/static/"^ url.CodecUrl.path) |>
      respond oc
    | "/" | "/index.html" ->
      make_index_html params |>
      http_msg_of_html |>
      respond oc
    | _ ->
      Printf.fprintf oc
        "HTTP/1.0 404 Go away you bozo\r\n\
         Content-Length:0\r\n\r\n%!")
  | _ -> (* ignore that bozo *) ()


let on_all_err err =
  Printf.eprintf "Error: %a\n"
    (HttpParser.P.print_bad_result CodecHttp.Msg.print) err

let server ic' oc =
  if debug then Printf.printf "New connection! I'm so excited!!\n%!" ;
  let ic = Unix.descr_of_in_channel ic' in
  let rec loop stream =
    let parser_res =
      HttpParser.(p [] None Parsers.no_error_correction stream |> P.to_result) in
    if debug then Printf.printf "Received:\n%a\n%!"
      (HttpParser.P.print_result CodecHttp.Msg.print) parser_res ;
    match parser_res with
    | Ok (msg, stream') ->
      with_timing "answering queries" (fun () ->
        on_all_http_msg oc msg) ;
      loop stream'
    | Error err -> on_all_err err in
  loop (make_stream ic)

let server_or_kaputt ic oc =
  (* Do open the files once per process *)
  Printf.printf "New server, opening all files.\n%!" ;
  Array.iter (fun g ->
      let open Config in
      assert (Array.length g.Graph.files > 0) ;
      let total_bounds = { first_x = max_float ; last_x = min_float } in
      let update_total_bounds b =
        if b.first_x < total_bounds.first_x then
          total_bounds.first_x <- b.first_x ;
        if b.last_x > total_bounds.last_x then
          total_bounds.last_x <- b.last_x in
      Array.iter (function
        | File file ->
          File.update_info file ;
          update_total_bounds file.File.bounds
        | Expr _ -> ()) g.Graph.files ;
      (* Set expression bounds to total_bounds.
       * TODO: allow to enter bounds of expression from the command line *)
      Array.iter (function
        | File _ -> ()
        | Expr e ->
          (* from now on bounds of expressions are read only *)
          e.Expr.bounds <- total_bounds) g.Graph.files ;
      (* Now that we have field labels try to use them to arrange axis
       * labels: *)
      if g.Graph.y1_label = default_y_label then (
        match Array.find (function
          | File file ->
            Array.length file.File.y1_fields > 0
          | Expr _ -> true) g.Graph.files with
        | exception Not_found -> ()
        | File file -> g.Graph.y1_label <- file.File.y1_fields.(0).Field.pen.Pen.label
        | Expr expr -> g.Graph.y1_label <- expr.Expr.pen.Pen.label
      ) ;
      if g.Graph.y2_label = default_y_label then (
        match Array.find (function
          | File file ->
            Array.length file.File.y2_fields > 0
          | Expr _ -> true) g.Graph.files with
        | exception Not_found -> ()
        | File file -> g.Graph.y2_label <- file.File.y2_fields.(0).Field.pen.Pen.label
        | Expr expr -> g.Graph.y2_label <- expr.Expr.pen.Pen.label
      )
    ) !Config.graphs ;
  try server ic oc
  with e ->
    let str = Printexc.to_string e ^"\n"^
              Printexc.get_backtrace () in
    kaputt oc str

let start_server () =
  let addr = Unix.(ADDR_INET (inet_addr_of_string "127.0.0.1", port)) in
  (* Better flush all outputs before forking *)
  IO.flush_all () ;
  if !Config.open_browser && Unix.fork () = 0 then (
    let open Unix in
    (* Wait for the server to be ready and open the browser *)
    let rec wait_server n =
      if n > 0 then (
        try let ic, oc = open_connection addr in
            ignore_exceptions IO.close_in ic ;
            ignore_exceptions IO.close_out oc ;
            Printf.printf "Server is ready, launching browser\n"
        with Unix.Unix_error (ENOTCONN, _, _) as exc ->
          Printf.eprintf "Cannot connect: %s\n%!" (Printexc.to_string exc) ;
          sleepf 0.05 ;
          wait_server (n-1)
      ) in
    wait_server 10 ;
    let has_subst, cmd =
      String.replace ~str:Config.(global.open_browser_with) ~sub:"%port%" ~by:(string_of_int port) in
    if not has_subst then
      Printf.eprintf "Warning: no substitution took place in %S\n" cmd ;
    Printf.printf "Running %S\n" cmd ;
    match system cmd with
    | WEXITED 0 -> ()
    | WEXITED ret ->
      Printf.eprintf "Process %S exited with status %d\n" cmd ret
    | WSIGNALED _ | WSTOPPED _ ->
      Printf.eprintf "Process %S killed by signal :(\n" cmd
  ) else (
    Printf.eprintf "Start HTTP server on port %d...\n%!" port ;
    Unix.establish_server ~cleanup:false server_or_kaputt addr
  )

let () =
  let open Config in
  parse_args Sys.argv ;
  if !output_svg then (
    let g = !graphs.(0) in
    assert (Array.length g.Graph.files > 0) ;
    Array.iter (function
      | File file -> Config.File.update_info file
      | Expr _ -> ()) g.Graph.files ;
    let bounds = Source.bounds g.Graph.files.(0) in
    let t1 = bounds.first_x
    and t2 = bounds.last_x
    and n = 100 in
    let svg = get_svg g n t1 t2 in
    Html.print stdout svg ;
    exit 0
  ) else (
    start_server ()
  )
