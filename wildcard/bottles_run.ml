open GT
open OCanren
open OCanren.Std
open Tester
open Bottles

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
  | s, b ->
    Printf.sprintf "(%s|%s)" (show logic show_bottle b) (show logic show_stepType s)
;;

let myshow = show List.logic @@ show logic show_step

(******************************************)

let rec int2nat n = if n = 0 then o () else s @@ int2nat @@ (n - 1)

(** For high order conversion **)

let run_exn eta = run_r (List.reify (Std.Pair.reify reify reify)) eta
let to_ground_bottle a = conde [ a === fst_ (); a === snd_ () ]
let to_ground_type a = conde [ a === fill (); a === empty (); a === pour () ]

let rec to_ground_list a =
  conde
    [ a === nil ()
    ; fresh
        (x y xs)
        (a === pair x y % xs)
        (to_ground_type x)
        (to_ground_bottle y)
        (to_ground_list xs)
    ]
;;

let _ =
  run_exn
    myshow
    1
    q
    qh
    ( "answers"
    , fun q -> fresh () (checkAnswer q capacities1 (int2nat 7) !!true) (to_ground_list q)
    )
;;
