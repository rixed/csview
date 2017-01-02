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

(* TODO: optional stack trace? *)
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
      "Content-Type", "text/html" ;
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
  let get n = CodecUrl.get_single_query_param params n in
  let to_opt f = try Some (f ()) with Not_found -> None in
  let default v f = try f () with Not_found -> v in
  let file = get "file" in
  let t1 = (to_opt (fun () -> get "t1" |> float_of_string) |?
            Read_csv.oldest file)
  and t2 = (to_opt (fun () -> get "t2" |> float_of_string) |?
            Read_csv.latest file)
  and n = default 100 (fun () -> get "n" |> int_of_string)
  in
  let g = Read_csv.get_graph file t1 t2 n in
  (* Let's forget about what we asked and use the actual number instead: *)
  let n = Array.length g.Read_csv.ts in
  Printf.eprintf "Computing SVG\n%!" ;
  (* The fold function is supposed to accumulate over all datasets *)
  let fold = { Chart.fold = fun f init ->
    List.fold_left (fun prev d ->
      (* We must pass a getter to f *)
      let get i =
        Printf.eprintf "%d->%f\n%!" i d.(i) ;
        d.(i) in
      f prev file true (* on the left Y-axis *) get)
      init g.Read_csv.ds } in
  let vx_step = (t2-.t1) /. float_of_int (n-1) in
  let svg = Chart.xy_plot "time" "value" t1 vx_step n fold in
  let msg = http_msg_of_svg svg in
  respond oc msg

let make_index_html params =
  Html.(html [] [ p [ cdata "hohoho" ] ])

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
    | "/favico.ico" ->
      http_msg_of_file "static/index.html" |>
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
  (* simple server: *)
  let addr = Unix.(ADDR_INET (inet_addr_of_string "127.0.0.1", port)) in
  Unix.establish_server server_or_kaputt addr
