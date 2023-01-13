open OCanren
open Tester
open Maybe_test.HO

let _ =
  let run x = run_r reify (GT.show logic @@ GT.show GT.string) x in
  run (-1) q qh ("Success test", fun q -> f (( === ) !!"ololo") q);
  run (-1) q qh ("Failure test", fun q -> g (( === ) !!"ololo") q);
  run (-1) q qh ("Pair test", fun q -> h (( === ) @@ Std.pair !!"ololo1" !!"ololo1") q)
;;
