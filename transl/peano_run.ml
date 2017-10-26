open MiniKanren
open Tester

let show_nat        = GT.show(Nat.ground)

let o = inj @@ Peano.For_gnumber.distrib Peano.O
(*let s self = inj Peano.(For_gnumber.distrib (S self))*)

let (_:int) = o
let () =
  run_exn show_nat (-1) q  qh ("incr ? 2", (fun q     -> Peano.(incr q (s (s o))  )) )
