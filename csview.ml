open Batteries

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
      version = 1, 0 ;
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
      version = 1, 0 ;
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
  let _, ext = String.rsplit f ~by:"." in
  if ext = "html" then "text/html"
  else "text/plain"

let http_msg_of_file fname =
  let body = read_whole_file fname in
  CodecHttp.(Msg.{
    start_line = StartLine.Response StatusLine.{
      version = 1, 0 ;
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
  let gi = default 0 (fun () -> get "gi" |> int_of_string) in
  let g = !graphs.(gi) in
  let t1 = default g.files.(0).first_x (fun () -> get "t1" |> float_of_string)
  and t2 = default g.files.(0).last_x (fun () -> get "t2" |> float_of_string)
  and n = default 100 (fun () -> get "n" |> int_of_string)
  in
  Printf.eprintf "Reading data\n" ;
  (* TODO: fold the SVG for all files fi *)
  let f_idx = 0 in
  let file = g.files.(f_idx) in
  let data = Read_csv.read_all file.fd file.x_field.index file.separator file.x_field.to_value file.block_size file.size n t1 t2 in
  (* data is an array of n data points (or less), where each data point is:
     ts : float * line : string *)
  (* Let's forget about what we asked and use the actual number instead: *)
  let n = Array.length data in
  Printf.eprintf "Computing SVG\n" ;
  (* The fold function is supposed to accumulate over all datasets *)
  let fold = { Chart.fold = fun f init ->
    let field_getter field i =
      let _, line = data.(i) in
      let y, _ = Read_csv.extract_field line file.separator 0 field.index field.to_value in
      Printf.eprintf "%d->%f\n" i y ;
      y in
    let init' = Array.fold_left (fun prev field ->
      f prev field.label true (* left Y-axis *) (field_getter field))
      init file.y1_fields in
    Array.fold_left (fun prev field ->
      f prev field.label false (* right Y-axis *) (field_getter field))
      init' file.y2_fields
    (* TODO: the annotations *)
    } in
  (* TODO: add other files to this SVG, without the axis *)
  let vx_step = (t2-.t1) /. float_of_int (n-1) in
  let svg =
    Chart.xy_plot ~svg_width:(float_of_int g.width)
                  ~svg_height:(float_of_int g.height) "time" "value"
                  t1 vx_step n fold in
  let msg = http_msg_of_svg svg in
  respond oc msg

let make_index_html _params =
  (* Can't be static because it depends on the number of graphs.
   * But we will refer to the static JS *)
  let html_of_graph g i =
    let attrs = [
      "width", string_of_int g.Config.width ;
      "height", string_of_int g.Config.height ] in
    Html.img ~attrs ("/graph.svg?g="^ string_of_int i) in
  let html_of_graphs gs =
    let rec loop prev i =
      if i >= Array.length gs then List.rev prev
      else loop (html_of_graph gs.(i) i :: prev) (i+1) in
    loop [] 0 in
  Html.(html [
    tag "script" ~attrs:["src", "/static/csview.js"] [] ]
    (html_of_graphs !Config.graphs))

let on_all_http_msg oc msg =
  match msg.CodecHttp.Msg.start_line with
  | CodecHttp.StartLine.Request r ->
    let url =
      r.CodecHttp.RequestLine.url |>
      CodecUrl.of_string in
    Printf.printf "answering to %s...\n%!" (CodecUrl.to_string url) ;
    let params = CodecUrl.parse_query_of_url url in
    (match url.CodecUrl.path with
    | "/graph.svg" ->
      get_graph oc params
    | "/static/favicon.ico" | "/static/csview.js" ->
      http_msg_of_file ("./"^ url.CodecUrl.path) |>
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
  Printf.printf "New connection! I'm so excited!!\n%!" ;
  let ic = Unix.descr_of_in_channel ic' in
  let rec loop stream =
    let parser_res =
      HttpParser.(p [] None Parsers.no_error_correction stream |> P.to_result) in
    Printf.printf "Received:\n%a\n%!"
      (HttpParser.P.print_result CodecHttp.Msg.print) parser_res ;
    match parser_res with
    | Ok (msg, stream') ->
      on_all_http_msg oc msg ;
      loop stream'
    | Bad err -> on_all_err err in
  loop (make_stream ic)

let server_or_kaputt ic oc =
  try server ic oc
  with e ->
    let str = Printexc.to_string e ^"\n"^
              Printexc.get_backtrace () in
    kaputt oc str

let () =
  Printf.eprintf "Parse command line...\n%!" ;
  Config.parse_args Sys.argv ;
  Printf.eprintf "Check the config and open all files...\n%!" ;
  Array.iter (fun g ->
      Array.iter Read_csv.update_file_info g.Config.files
    ) !Config.graphs ;
  Printf.eprintf "Start HTTP server on port %d...\n%!" port ;
  let addr = Unix.(ADDR_INET (inet_addr_of_string "127.0.0.1", port)) in
  Unix.establish_server server_or_kaputt addr
