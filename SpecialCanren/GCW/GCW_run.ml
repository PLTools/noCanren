open OCanren
open OCanren.Std
open Tester
open GCW

(*************************************************)
let show_ans x = [%show: person_logic List.logic] () x
let reify_ans x = List.reify person_reify x
let _ = run_r reify_ans show_ans 100 q qh ("answers", fun q -> checkAnswer q !!true)
