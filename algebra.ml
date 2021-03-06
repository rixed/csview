(* Allow an algebraic expression to replace a csv file.
 * On the command line, if a file name cannot be opened, try to parse
 * it as an algebraic expression (using parsercombinators lib) and build
 * a function from float to float. *)
open Batteries
module PConfig = Parsers.SimpleConfig (Char)
module P = Parsers.Make (PConfig)
module U = ParsersUsual.Make (P)

open P
open U

let n = number >>: function
  | Int i -> Num.float_of_num i
  | Float f -> f
let x = item ~what:"x" 'x'

type value = Fun of (float -> float) | Imm of float
let value =
  (x >>: fun _ -> Fun identity) |||
  (n >>: fun v -> Imm v)

let operation =
  let sep = opt_whitespace >>: fun _ -> () in
  let reduce a op b =
    let op_f = match op with
      | '+' -> (+.)
      | '-' -> (-.)
      | '*' -> ( *.)
      | '/' -> (/.)
      | '^' -> ( **)
      | _   -> assert false in
    match a, b with
      | Imm a, Imm b -> Imm (op_f a b)
      | Imm a, Fun f -> Fun (fun x -> op_f a (f x))
      | Fun f, Imm b -> Fun (fun x -> op_f (f x) b)
      | Fun f, Fun g -> Fun (fun x -> op_f (f x) (g x)) in
  let unary_op m =
    ((string "sin" >>: fun _ -> sin) |||
     (string "cos" >>: fun _ -> cos) |||
     (string "tan" >>: fun _ -> tan) |||
     (string "asin" >>: fun _ -> asin) |||
     (string "acos" >>: fun _ -> acos) |||
     (string "atan" >>: fun _ -> atan) |||
     (string "sinh" >>: fun _ -> sinh) |||
     (string "cosh" >>: fun _ -> cosh) |||
     (string "tanh" >>: fun _ -> tanh) |||
     (string "log" >>: fun _ -> log10) |||
     (string "ln"  >>: fun _ -> log) |||
     (string "exp" >>: fun _ -> exp) |||
     (string "sqrt">>: fun _ -> sqrt)) m in
  let rec left_assoc_low_prec m =
    binary_ops_reducer ~op:(item '+' ||| item '-')
                       ~term:left_assoc_high_prec
                      ~sep ~reduce m
  and left_assoc_high_prec m =
    binary_ops_reducer ~op:(item '*' ||| item '/')
                       ~term:right_assoc_higher_prec
                       ~sep ~reduce m
  and right_assoc_higher_prec m =
    binary_ops_reducer ~op:(item '^')
                       ~right_associative:true
                       ~term:left_assoc_highest_prec
                       ~sep ~reduce m
  and left_assoc_highest_prec m =
    (value |||
     (optional ~def:identity unary_op +- sep +-
      item '(' ++ left_assoc_low_prec +- item ')' >>: function
        | (f, Imm v) -> Imm (f v)
        | (f, Fun g) -> Fun (fun x -> f (g x)))) m in
  left_assoc_low_prec

(*$= operation & ~printer:(IO.to_string (P.print_result Float.print))
  (Ok (2.,(3,[]))) (parse_with operation "1+1")
  (Ok (2.,(4,[]))) (parse_with operation "1 +1")
  (Ok (2.,(4,[]))) (parse_with operation "1+ 1")
  (Ok (2.,(5,[]))) (parse_with operation "1 + 1")
  (Ok (5.,(8,[]))) (parse_with operation "2.5 *  2")
  (Ok (~-.39.,(11,[]))) (parse_with operation "1 - 2e1 * 2")
  (Ok (4.,(12,[]))) (parse_with operation "(1.0 *2) ^ 2")
*)

let stream_of_string = PConfig.stream_of_string
