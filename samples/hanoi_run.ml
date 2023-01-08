open GT
open OCanren
open OCanren.Std
open Tester
open Hanoi

(*************************************************)

let show_stick = function
  | One -> "One"
  | Two -> "Two"
  | Thr -> "Thr"
;;

let show_lstick = show logic show_stick
let show_answer = show List.ground @@ show Pair.ground show_stick show_stick
let lshow_answer = show List.logic @@ show Pair.logic show_lstick show_lstick
let reify_answer = List.reify (Pair.reify OCanren.reify OCanren.reify)
let run x = run_r reify_answer lshow_answer x

(*************************************************)

let rec toN n = if n = 0 then z () else s (toN (n - 1))

let gen_pin n =
  let rec gen_pin m = if m = n then nil () else toN m % gen_pin (m + 1) in
  gen_pin 0
;;

let gen n = triple (gen_pin n) (nil ()) (nil ())

(** For high order conversion **)
let check_o p q r = check_o (( === ) p) (( === ) q) r

let _ = run 1 q qh ("first", fun q -> check_o (gen 3) q !!true)
