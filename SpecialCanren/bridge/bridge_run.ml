open GT
open OCanren
open OCanren.Std
open Tester
open Bridge

(*************************************************)

let show_step f = function
  | One x -> f x
  | Two (x, y) -> Printf.sprintf "(%s, %s)" (f x) (f y)
;;

let show_ans x = show List.logic (show logic (show_step (show logic show_person))) x
let reify_ans x = List.reify step_reify x

(*************************************************)

let rec int2nat i = if i = 0 then !!O else !!(S (int2nat @@ (i - 1)))

let _ =
  run_r reify_ans show_ans 1 q qh ("answers", fun q -> getAnswer q (int2nat 17 |> some))
;;
