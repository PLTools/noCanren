open GT
open OCanren
open OCanren.Std
open Tester
open GCW.HO

(*************************************************)

let show_person = function
  | G -> "G"
  | W -> "W"
  | C -> "C"
  | N -> "N"
;;

let myshow x = show List.ground show_person x

(*************************************************)

(** For high order conversion **)
let checkAnswer q r = checkAnswer (( === ) q) r

let _ =
  run_r
    (Std.List.prj_exn OCanren.prj_exn)
    myshow
    1
    q
    qh
    ("answers", fun q -> checkAnswer q !!true)
;;
