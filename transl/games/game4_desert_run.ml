open MiniKanren
open MiniKanrenStd
open Game4_desert
open Tester


let show_number num =
  let rec helper = function
  | O   -> 0
  | S x -> 1  + (helper x)
  in
  string_of_int @@ helper num

let show_step = function
  | Left x  -> Printf.sprintf "L%s" @@ show_number x
  | Right x -> Printf.sprintf "R%s" @@ show_number x
  | Fill    -> "F"
  | Pour x  -> Printf.sprintf "P%s" @@ show_number x


let show_list f x =
 let rec show_l = function
 | Nil           -> ""
 | Cons (x, Nil) -> f x
 | Cons (x, xs)  -> Printf.sprintf "%s; %s" (f x) (show_l xs)
 in
 Printf.sprintf "[%s]" (show_l x)

let rec of_int i = if i = 0 then o () else s @@ of_int @@ i - 1

let myshow x = show_list (show_step) x

let () =
(*run_exn myshow (1) q qh ("answers", (fun q -> (checkAnswer ((===)q) eig fiv (just @@ of_int 22))));*)
  run_exn myshow (1) q qh ("answers", (fun q -> call_fresh (fun x -> (checkAnswer ((===)q) eig fiv (just x)))));
  ()


