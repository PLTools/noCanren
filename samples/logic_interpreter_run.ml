open GT

open OCanren
open OCanren.Std
open Tester

open Logic_interpreter

(*************************************************)

let myshow x = show List.ground (show Pair.ground (show string) (show bool)) x

(*************************************************)
(** For high order conversion **)
(* let check_and_eval q e r = check_and_eval ((===) q) ((===) e) r *)

let _ =
  run_exn myshow (-1) q qh ("answers", fun q -> fresh (e) (expr1 e) (check_and_eval q e (some !!true)));
  run_exn myshow (-1) q qh ("answers", fun q -> fresh (e) (expr2 e) (check_and_eval q e (some !!true)));
  run_exn myshow (-1) q qh ("answers", fun q -> fresh (e) (expr3 e) (check_and_eval q e (some !!true)));
  run_exn myshow (-1) q qh ("answers", fun q -> fresh (e) (expr4 e) (check_and_eval q e (some !!true)))
