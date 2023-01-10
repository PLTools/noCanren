open GT
open OCanren
open OCanren.Std
open Tester
open Logic_interpreter.HO

(*************************************************)

let reify x = List.reify (Pair.reify reify reify) x

let myshow x =
  show
    List.logic
    (show Pair.logic (show logic @@ show string) (show logic @@ show bool))
    x
;;

(*************************************************)

(** For high order conversion **)
let check_and_eval q e r = check_and_eval (( === ) q) (( === ) e) r

let _ =
  let run x = run_r reify myshow x in
  run (-1) q qh ("answers", fun q -> fresh e (expr1 e) (check_and_eval q e (some !!true)));
  run (-1) q qh ("answers", fun q -> fresh e (expr2 e) (check_and_eval q e (some !!true)));
  run (-1) q qh ("answers", fun q -> fresh e (expr3 e) (check_and_eval q e (some !!true)));
  run (-1) q qh ("answers", fun q -> fresh e (expr4 e) (check_and_eval q e (some !!true)))
;;
