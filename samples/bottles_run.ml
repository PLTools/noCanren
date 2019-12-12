open GT

open OCanren
open OCanren.Std
open Tester

open Bottles

(******************************************)

let show_bottle = function
 | Fst -> "1"
 | Snd -> "2"

let show_stepType = function
 | Fill  -> "F"
 | Empty -> "E"
 | Pour  -> "P"

let show_step = function
 | (s, b) -> Printf.sprintf "%s%s" (show_bottle b) (show_stepType s)

let myshow x = show List.ground show_step x

(******************************************)

let rec int2nat n = if n = 0 then o () else s @@ int2nat @@ n - 1

(** For high order conversion **)
let checkAnswer q c n r = checkAnswer ((===) q) c ((===) n) r

let _ =
  run_exn myshow (1) q qh ("answers", fun q ->
    checkAnswer q capacities1 (int2nat 7) !!true
  )
