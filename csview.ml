open Batteries
open Option.Infix

let debug = false

let port = 28019

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
  let body =
    (IO.to_string (fun oc () -> Html.print_xml_head oc) ()) ^
    (IO.to_string Html.print html) in
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
  let ic = open_in fname in
  let str = IO.read_all ic in
  close_in ic ;
  str

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

let get_graph oc params =
  let open Config in
  let get n = CodecUrl.get_single_query_param params n in
  let default v f = try f () with Not_found -> v in
  let gi = default 0 (fun () -> get "g" |> int_of_string) in
  let g = !graphs.(gi) in
  let t1 = default g.files.(0).first_x (fun () -> get "t1" |> float_of_string)
  and t2 = default g.files.(0).last_x (fun () -> get "t2" |> float_of_string)
  and n = default 100 (fun () -> get "n" |> int_of_string)
  in
  (* The fold function is supposed to accumulate over all datasets *)
  let fold = { Chart.fold = fun f init ->
    (* we want to iterate over all fields of all files of this graph *)
    let rec for_all_fields f_idx prev  =
      if f_idx >= Array.length g.files then prev else (
        let file = g.files.(f_idx) in
        (* We cannot open the file earlier because this is a forking server and
         * we do not want to share the file offsets with other processes *)
        let data = Read_csv.read_all file.fd file.x_field.index file.separator file.x_field.fmt.Formats.to_value file.block_size file.size n t1 t2 in
        (* data is an array of n data points, where each data point is:
           ts : float * line : string *)
        (* Let's forget about what we asked and use the actual number instead: *)
        let field_getter field i =
          let _, line = data.(i) in
          let y, _ = Read_csv.extract_field line file.separator 0 field.index field.fmt.Formats.to_value in
          y in
        let prev' = Array.fold_left (fun prev field ->
          f prev field.label true (* left Y-axis *) (field_getter field))
          prev file.y1_fields in
        let prev' = Array.fold_left (fun prev field ->
          f prev field.label false (* right Y-axis *) (field_getter field))
          prev' file.y2_fields in
        (* TODO: the annotations *)
        for_all_fields (f_idx+1) prev') in
    for_all_fields 0 init } in
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
    Array.fold_left (fun (fmt1, fmt2) file ->
        let fmt1 = match fmt1 with
          | None -> get_first_fmt file.y1_fields
          | _ -> fmt1
        and fmt2 = match fmt2 with
          | None -> get_first_fmt file.y2_fields
          | _ -> fmt2 in
        fmt1, fmt2
      ) (None, None) g.files in
  let svg =
    Chart.xy_plot ~string_of_x:g.files.(0).x_field.fmt.Formats.to_label
                  ?string_of_y ?string_of_y2
                  ~svg_width:(float_of_int (g.width |? global.default_width))
                  ~svg_height:(float_of_int (g.height |? global.default_height))
                  ~font_size:g.font_size
                  ~stacked_y1:g.y1_stacked
                  ~stacked_y2:g.y2_stacked
                  ~force_show_0:g.force_show_0
                  g.x_label g.y1_label t1 vx_step n fold in
  let msg = http_msg_of_svg svg in
  respond oc msg

let make_index_html _params =
  (* Can't be static because it depends on the number of graphs. *)
  let html_of_graph g i =
    let attrs = [
      "width", string_of_int Config.(g.width |? global.default_width) ;
      "height", string_of_int Config.(g.height |? global.default_height) ] in
    let id = "graph_"^ string_of_int i in
    let open Config in
    let t1 = g.x_start |? g.files.(0).first_x
    and t2 = g.x_stop  |? g.files.(0).last_x in
    let url = "/graph.svg?g="^ string_of_int i ^
              "&t1="^ string_of_float t1 ^"&t2="^ string_of_float t2 in
    Html.Block (
      (if g.title <> "" then [ Html.h2 g.title ] else []) @
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
      http_msg_of_file ("./static/"^ url.CodecUrl.path) |>
      respond oc
    | "/index.html" ->
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
      on_all_http_msg oc msg ;
      loop stream'
    | Bad err -> on_all_err err in
  loop (make_stream ic)

let server_or_kaputt ic oc =
  (* Do open the files once per process *)
  Printf.printf "New server, opening all files.\n%!" ;
  Array.iter (fun g ->
      assert (Array.length g.Config.files > 0) ;
      Array.iter Read_csv.update_file_info g.Config.files
    ) !Config.graphs ;
  try server ic oc
  with e ->
    let str = Printexc.to_string e ^"\n"^
              Printexc.get_backtrace () in
    kaputt oc str

let () =
  Printf.eprintf "Parse command line...\n%!" ;
  Config.parse_args Sys.argv ;
  Printf.eprintf "Check the config and open all files...\n%!" ;
  Printf.eprintf "Start HTTP server on port %d...\n%!" port ;
  let addr = Unix.(ADDR_INET (inet_addr_of_string "127.0.0.1", port)) in
  Unix.establish_server server_or_kaputt addr
