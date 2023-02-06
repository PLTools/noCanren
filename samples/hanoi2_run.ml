module L = List
open GT
open OCanren
open OCanren.Std
open Hanoi2.HO

(*************************************************)

let show_pin = function
  | A -> "A"
  | B -> "B"
  | C -> "C"
;;

let show_answer = show List.ground @@ show Pair.ground show_pin show_pin
let rec toN n = if n = 0 then !!Z else !!(S (toN (n - 1)))

let gen_pin n =
  let rec gen_pin m = if m = n then nil () else toN m % gen_pin (m + 1) in
  gen_pin 0
;;

let start n = ctor_gset (gen_pin n) (nil ()) (nil ())
let finish n = ctor_gset (nil ()) (nil ()) (gen_pin n)

let _ =
  Printf.printf "%s\n"
  @@ show_answer
  @@ L.hd
  @@ Stream.take ~n:1
  @@ run
       q
       (fun q -> eval (( === ) q) (( === ) (start 3)) (finish 3))
       (fun rr -> rr#reify (Std.List.prj_exn (Std.Pair.prj_exn prj_exn prj_exn)))
;;
