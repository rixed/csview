(* A trivial serie for tests: *)

let test_ts = [| 1.; 2.; 3.; 4. |]

let test_data_1_csv =
  Unix.(openfile "test_data/1.csv" [O_RDONLY] 0o644)

