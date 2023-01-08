(* open GT *)

open OCanren
open OCanren.Std
open Tester
open GCW

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

(** For call-by-need conversion **)
(* let checkAnswer q r = (snd checkAnswer) ([Obj.magic q], (===) q) r *)

let _ = run_exn myshow 1 q qh ("answers", fun q -> checkAnswer q !!true)
