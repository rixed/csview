(* Allow an algebraic expression to replace a csv file.
 * On the command line, if a file name cannot be opened, try to parse
 * it as an algebraic expression (using parsercombinators lib) and build
 * a function from float to float. *)
open Batteries
module P = Parsers.Make (Parsers.SimpleConfig (Char))
module U = ParsersUsual.Make (P)

open P
open U

let n = number >>: function
  | Int i -> float_of_int i
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
     item '(' -+ left_assoc_low_prec +- item ')') m in
  left_assoc_low_prec

(*$= operation & ~printer:(IO.to_string (P.print_result Float.print))
  (Ok (2.,[])) (parse_with operation "1+1")
  (Ok (2.,[])) (parse_with operation "1 +1")
  (Ok (2.,[])) (parse_with operation "1+ 1")
  (Ok (2.,[])) (parse_with operation "1 + 1")
  (Ok (5.,[])) (parse_with operation "2.5 *  2")
  (Ok (~-.39.,[])) (parse_with operation "1 - 2e1 * 2")
  (Ok (4.,[])) (parse_with operation "(1.0 *2) ^ 2")
*)

let stream_of_string s =
  let rec loop n tl =
    if n < 0 then tl
    else loop (n-1) ((n, s.[n]) :: tl) in
  loop (String.length s - 1) []

