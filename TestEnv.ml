open Batteries

let test_data_1_csv =
  Unix.(openfile "test_data/1.csv" [O_RDONLY] 0o644)

(* for parsing algebraic expression *)

let parse_with p ?(x=0.) str =
  let e = ParsersBoundedSet.make 0 in
  let s = Algebra.stream_of_string str in
  let open Algebra.P in
  let p =
    (* force parsing the whole string and apply the result function at x *)
    p +- eof >>: function
      | Algebra.Imm v -> v
      | Algebra.Fun f -> f x in
  p [] None e s |> to_result

