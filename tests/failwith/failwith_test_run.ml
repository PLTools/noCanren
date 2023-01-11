open OCanren
open OCanren.Std
open Tester
open Failwith_test.HO

let _ =
  let run x =
    run_r
      (Pair.reify Bool.reify Bool.reify)
      (GT.show Pair.logic (GT.show Bool.logic) (GT.show Bool.logic))
      (-1)
      q
      qh
      x
  in
  run ("Failwith test 1", fun q -> fresh (a b) (q === pair a b) (only_true (( === ) a) b));
  run ("Failwith test 2", fun q -> fresh (a b) (q === pair a b) (only_false (( === ) a) b));
  run ("Failwith test 3", fun q -> fresh (a b) (q === pair a b) (is_failure (( === ) a) b))
;;
