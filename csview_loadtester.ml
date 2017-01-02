open Batteries

module ParserConfig = ParsersConfig.BlockList (ParsersConfig.FileReader)
module ParserConfigWithOffset = ParsersPositions.Offset (ParserConfig)
let make_stream ic = ParserConfig.make_stream ic, 0
module HttpParser = CodecHttp.MakeParser (ParserConfigWithOffset)

(* Typical chrome greeting to an unknown server on my machine: *)
let single_http_msg port = CodecHttp.(Msg.{
  start_line = StartLine.Request RequestLine.{
    cmd = Command.GET ; url = "/whatever" ; version = 1,1 } ;
  headers = [
    "Host", "localhost:"^ string_of_int port ;
    "Connection", "keep-alive" ;
    "Pragma", "no-cache" ;
    "Cache-Control", "no-cache" ;
    "Upgrade-Insecure-Requests", "1" ;
    "User-Agent", "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/55.0.2883.75 Safari/537.36" ;
    "Accept", "text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,*/*;q=0.8" ;
    "DNT", "1" ;
    "Accept-Encoding", "gzip, deflate, sdch, br" ;
    "Accept-Language", "en-US,en;q=0.8,fr;q=0.6" ;
    "Cookie", "org.cups.sid=08b425b767ab62b25dae4fb76b9fdd8a"
  ] ;
  body = "" })

let () =
  ignore Sys.(signal sigpipe Signal_ignore) ;
  let open Unix in
  let port = ref 28019 in (* TODO cmd line me please *)
  let n = ref 1000 in
  let domain = PF_INET in (* TODO: cmdline me please *)
  let addr = ADDR_INET (inet_addr_of_string "127.0.0.1", !port) in
  let s = socket domain SOCK_STREAM 0 in
  Arg.(parse [
    "-n", Set_int n, "number of queries to send" ;
    "-port", Set_int port, "destination port" ;
  ] (fun s -> raise (Bad s)) "csview_loadtester.opt: load test csview") ;
  let str = CodecHttp.Msg.encode (single_http_msg !port) in
  let len = String.length str in
  connect s addr ;
  let rec loop stream i =
    if i >= !n then (
      Printf.printf "Done writing %d msgs\n%!" i ;
      i
    ) else (
      if write s str 0 len != len then failwith "Cannot write" ;
      let open HttpParser in
      match p [] None Parsers.no_error_correction stream |> P.to_result with
      | Ok (_, stream') -> loop stream' (i+1)
      | Bad some_err ->
        Printf.eprintf "Cannot parse output: %a\n%!"
          (P.print_bad_result CodecHttp.Msg.print) some_err ;
        i
    ) in
  let ok =
    (try loop (make_stream s) 0
     with _ -> Printf.eprintf "Oups!\n%!" ; -1) in
  Printf.printf "ok=%d\n%!" ok ;
  (try shutdown s SHUTDOWN_ALL
   with Unix.Unix_error (ENOTCONN, _, _) -> ())

