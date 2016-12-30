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

let on_all_http_msg oc _msg =
  Printf.printf "answering...\n%!" ;
  Printf.fprintf oc "HTTP/1.0 200 Kaputt\r\nContent-Length:0\r\n\r\n%!"

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

let () =
  (* simple server: *)
  let addr = Unix.(ADDR_INET (inet_addr_of_string "127.0.0.1", port)) in
  Unix.establish_server server addr
