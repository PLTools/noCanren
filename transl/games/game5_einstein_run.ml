open MiniKanren
open MiniKanrenStd
open Game5_einstein
open Tester

open GT

let show_hause_color = function
  | Yellow       -> "Yellow      "
  | Blue         -> "Blue        "
  | Red          -> "Red         "
  | Ivory        -> "Ivory       "
  | Green        -> "Green       "

let show_nationality = function
  | Norwegian    -> "Norwegian   "
  | Ukrainian    -> "Ukrainian   "
  | Englishman   -> "Englishman  "
  | Spaniard     -> "Spaniard    "
  | Japanese     -> "Japanese    "

let show_drink = function
  | Water        -> "Water       "
  | Tea          -> "Tea         "
  | Milk         -> "Milk        "
  | Orange_juice -> "Orange_juice"
  | Coffee       -> "Coffee      "

let show_smoke = function
  | Kools        -> "Kools       "
  | Chesterfield -> "Chesterfield"
  | Old_Gold     -> "Old_Gold    "
  | Lacky_Strike -> "Lacky_Strike"
  | Parliament   -> "Parliament  "

let show_pet = function
  | Fox          -> "Fox         "
  | Hourse       -> "Hourse      "
  | Snails       -> "Snails      "
  | Dog          -> "Dog         "
  | Zebra        -> "Zebra       "

let show_person = function
  | Person (c, n, d, s, p) -> Printf.sprintf "%s + %s + %s + %s + %s" (show_hause_color c) (show_nationality n) (show_drink d) (show_smoke s) (show_pet p)


let show_state = function
  | State (p1, p2, p3, p4, p5) -> Printf.sprintf "\n  %s\n  %s\n  %s\n  %s\n  %s\n" (show_person p1) (show_person p2) (show_person p3) (show_person p4) (show_person p5)

let myshow = show_state

let show_bool b = if b then "true" else "false"

let () =
  run_exn myshow (-1) q qh ("answers", (fun q -> (check_state ((===) q) (!!true))));
  ()
