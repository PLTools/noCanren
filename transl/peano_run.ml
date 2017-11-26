open MiniKanren
open Tester

let show_nat        = GT.show(Nat.ground)
let rec show_number num =
  let rec helper = function
  | Peano.O -> 0
  | Peano.S x -> 1  + (helper x)
  in
  string_of_int @@ helper num


(*let o = inj @@ Peano.For_gnumber.distrib Peano.O*)
(*let s self = inj Peano.(For_gnumber.distrib (S self))*)

let () =
  run_exn show_number (-1) q  qh ("incr ? 2", (fun q     -> Peano.(Peano.incr ((===)q) (s (s @@ o ()))  )) )
