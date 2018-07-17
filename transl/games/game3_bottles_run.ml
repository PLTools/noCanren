open MiniKanren
open MiniKanrenStd
open Game3_bottles
open Tester


let show_bottle = function
 | Fst -> "1"
 | Snd -> "2"

let show_stepType = function
 | Fill  -> "F"
 | Empty -> "E"
 | Pour  -> "P"


let show_step = function
 | Step (s, b) -> Printf.sprintf "%s%s" (show_bottle b) (show_stepType s)

let show_list f x =
 let rec show_l = function
 | Nil           -> ""
 | Cons (x, Nil) -> f x
 | Cons (x, xs)  -> Printf.sprintf "%s; %s" (f x) (show_l xs)
 in
 Printf.sprintf "[%s]" (show_l x)

let myshow x = show_list (show_step) x

let () =
 run_exn myshow (-1) q qh ("answers", (fun q -> checkAnswer q capacities1 (s (s (s (s (s (s (o ()))))))) (!!true)));
 ()
