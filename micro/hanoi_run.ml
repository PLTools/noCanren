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
let show_answer = show LList.ground @@ show LPair.ground show_stick show_stick
let lshow_answer = show LList.logic @@ show LPair.logic show_lstick show_lstick
let reify_answer x = LList.reify (LPair.reify OCanren.reify OCanren.reify) x
let run x = runR reify_answer show_answer lshow_answer x

(*************************************************)

let rec toN n = if n = 0 then z () else s (toN (n - 1))

let gen_pin n =
  let rec gen_pin m = if m = n then nil () else toN m % gen_pin (m + 1) in
  gen_pin 0
;;

let gen n = triple (gen_pin n) (nil ()) (nil ())

(** For high order conversion **)
let check p q r = check (( === ) p) (( === ) q) r

(** For call-by-need conversion **)
(* let check p q r = (snd check) ([], (===) p) ([Obj.magic q], (===) q) r *)

let _ = run 1 q qh ("first", fun q -> check (gen 3) q !!true)
