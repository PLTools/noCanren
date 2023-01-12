open GT
open OCanren
open OCanren.Std
open Tester
open Hanoi.HO

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

let gen_pin n =
  let rec gen_pin m = if m = n then nil () else Peano.HO.from_int m % gen_pin (m + 1) in
  gen_pin 0
;;

let gen n = Std.pair (gen_pin n) @@ Std.pair (nil ()) (nil ())

(** For high order conversion **)
let check p q r = check (( === ) p) (( === ) q) r

let _ = run 1 q qh ("first", fun q -> check (gen 3) q !!true)
