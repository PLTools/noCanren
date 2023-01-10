open OCanren
open Tester
open Peano_test.HO

let _ =
  let run x = run_r Std.Nat.reify (GT.show Std.Nat.logic) x in
  run (-1) q qh ("Peano test", fun q -> is_ten q);
  run (-1) q qh ("Negative test", fun q -> is_negative q)
;;
