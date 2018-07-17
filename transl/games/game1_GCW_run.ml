open MiniKanren
open MiniKanrenStd
open Game1_GCW
open Tester


let show_person = function
  | G -> "G"
  | W -> "W"
  | C -> "C"
  | N -> "N"

let show_list f x =
  let rec show_l = function
  | Nil -> ""
  | Cons (x, Nil) -> f x
  | Cons (x, xs) -> Printf.sprintf "%s, %s" (f x) (show_l xs)
  in
  Printf.sprintf "[%s]" (show_l x)

let myshow x = show_list show_person x

let () =
  run_exn myshow (-1) q  qh  ("answers", (fun q -> checkAnswer q (!!true)) );
  ()
