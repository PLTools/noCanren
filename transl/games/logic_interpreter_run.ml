open MiniKanren
open MiniKanrenStd
open Logic_interpreter
open Tester

let show_list f x =
 let rec show_l = function
 | Nil           -> ""
 | Cons (x, Nil) -> f x
 | Cons (x, xs)  -> Printf.sprintf "%s; %s" (f x) (show_l xs)
 in
 Printf.sprintf "[%s]" (show_l x)

let show_pair f g = function
 | Pair (a, b) -> Printf.sprintf "%s = %s" (f a) @@ g b

let show_bool b = if b then "true" else "false"

let myshow x = show_list (show_pair (fun x -> x) show_bool) x


let () =
  run_exn myshow (-1) q qh ("answers", (fun q -> check_and_eval ((===)q) expr1 (just (!!true))));
  run_exn myshow (-1) q qh ("answers", (fun q -> check_and_eval ((===)q) expr2 (just (!!true))));
  run_exn myshow (-1) q qh ("answers", (fun q -> check_and_eval ((===)q) expr3 (just (!!true))));
  run_exn myshow (-1) q qh ("answers", (fun q -> check_and_eval ((===)q) expr4 (just (!!true))));
  ()
