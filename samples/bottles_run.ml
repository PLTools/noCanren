open GT
open OCanren
open OCanren.Std
open Tester
open Bottles
open Bottles.HO

(******************************************)

let show_bottle = function
  | Fst -> "1"
  | Snd -> "2"
;;

let show_stepType = function
  | Fill -> "F"
  | Empty -> "E"
  | Pour -> "P"
;;

let show_step = function
  | s, b -> Printf.sprintf "%s%s" (show_bottle b) (show_stepType s)
;;

let myshow x = show List.ground show_step x

(******************************************)

let run_exn eta =
  run_r (List.prj_exn (Std.Pair.prj_exn OCanren.prj_exn OCanren.prj_exn)) eta
;;

let _ =
  run_exn
    myshow
    1
    q
    qh
    ("answers", fun q -> FO.checkAnswer q FO.capacities1 (nat 7) !!true)
;;
