open OCanren
open OCanren.Std
open Tester
open Einstein.HO

(*************************************************)

let show_hause_color = function
  | Yellow -> "Yellow      "
  | Blue -> "Blue        "
  | Red -> "Red         "
  | Ivory -> "Ivory       "
  | Green -> "Green       "
;;

let show_nationality = function
  | Norwegian -> "Norwegian   "
  | Ukrainian -> "Ukrainian   "
  | Englishman -> "Englishman  "
  | Spaniard -> "Spaniard    "
  | Japanese -> "Japanese    "
;;

let show_drink = function
  | Water -> "Water       "
  | Tea -> "Tea         "
  | Milk -> "Milk        "
  | Orange_juice -> "Orange_juice"
  | Coffee -> "Coffee      "
;;

let show_smoke = function
  | Kools -> "Kools       "
  | Chesterfield -> "Chesterfield"
  | Old_Gold -> "Old_Gold    "
  | Lacky_Strike -> "Lacky_Strike"
  | Parliament -> "Parliament  "
;;

let show_pet = function
  | Fox -> "Fox         "
  | Hourse -> "Hourse      "
  | Snails -> "Snails      "
  | Dog -> "Dog         "
  | Zebra -> "Zebra       "
;;

let show_person = function
  | c, (n, (d, (s, p))) ->
    Printf.sprintf
      "%s + %s + %s + %s + %s"
      (show_hause_color c)
      (show_nationality n)
      (show_drink d)
      (show_smoke s)
      (show_pet p)
;;

let show_state = function
  | p1, (p2, (p3, (p4, p5))) ->
    Printf.sprintf
      "\n  %s\n  %s\n  %s\n  %s\n  %s\n"
      (show_person p1)
      (show_person p2)
      (show_person p3)
      (show_person p4)
      (show_person p5)
;;

let myshow = show_state

let reify_state () =
  (* right associative *)
  let ( ** ) = Std.Pair.prj_exn in
  let person = prj_exn ** prj_exn ** prj_exn ** prj_exn ** prj_exn in
  person ** person ** person ** person ** person
;;

(*************************************************)

(** For high order conversion **)
let check_state q r = check_state (( === ) q) r

let run_exn eta = run_r (reify_state ()) eta
let _ = run_exn myshow 1 q qh ("answers", fun q -> check_state q !!true)
