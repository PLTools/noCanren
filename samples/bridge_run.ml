open GT
open OCanren
open OCanren.Std
open Tester
open Bridge
open Bridge.HO

(*************************************************)

let show_step f = function
  | One x -> f x
  | Two (x, y) -> Printf.sprintf "(%s, %s)" (f x) (f y)
;;

let myshow x = show List.ground (show_step show_person) x

(*************************************************)

let _ =
  run_r
    (Std.List.prj_exn step_prj_exn)
    myshow
    1
    q
    qh
    ("answers", fun q -> FO.getAnswer q FO.standartTimes (nat 17))
;;
