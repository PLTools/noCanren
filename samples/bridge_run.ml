open GT

open OCanren
open OCanren.Std
open Tester

open Bridge

(*************************************************)

let show_person = function
 | A -> "A"
 | B -> "B"
 | C -> "C"
 | D -> "D"

let show_step f = function
 | One x     -> f x
 | Two (x,y) -> Printf.sprintf "(%s, %s)" (f x) (f y)

let myshow x = show List.ground (show_step show_person) x

(*************************************************)

let rec int2nat i = if i = 0 then o () else s @@ int2nat @@ i - 1

(** For high order conversion **)
let getAnswer_o q t r = getAnswer_o ((===) q) t r

let _ =
  run_exn myshow (1) q qh ("answers", fun q ->
    getAnswer_o q standartTimes_o (int2nat 17 |> some)
  )
