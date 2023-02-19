open Peano

let f = 2 * 2

[@@@ocaml
module L = Stdlib.List
open OCanren
open OCanren.Std
open HO

let _ =
  run q (fun q -> FO.f q) (fun rr -> rr#reify Nat.prj_exn)
  |> Stream.take
  |> L.iter (fun n -> Printf.printf "%s" @@ GT.show Nat.ground n)
;;]
