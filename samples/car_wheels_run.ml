open OCanren
open OCanren.Std
open Tester
open Peano.HO
open Car_wheels

(************************************************************************************************)

let _ =
  Printf.printf "Functional version \n%!";
  Printf.printf "Total distance: %d\n%!" @@ eval 3 4 [ 1, (0, 1, 2, 3); 1, (2, 3, 0, 1) ];
  Printf.printf "\n\n%!"
;;

(************************************************************************************************)
(************************************************************************************************)
(************************************************************************************************)

let show_nat x = GT.show GT.int @@ Nat.to_int x

let show_wheels (a, (b, (c, d))) =
  Printf.sprintf
    "(front: %s, %s; back: %s, %s)"
    (show_nat a)
    (show_nat b)
    (show_nat c)
    (show_nat d)
;;

let show (distance, steps) =
  Printf.sprintf
    "Total distance: %s; Steps: %s"
    (show_nat distance)
    (GT.show List.ground (GT.show Pair.ground show_nat show_wheels) steps)
;;

(************************************************************************************************)

let reify_wheel x = Nat.prj_exn x

let reify_wheels x =
  Pair.prj_exn
    reify_wheel
    (Pair.prj_exn reify_wheel (Pair.prj_exn reify_wheel reify_wheel))
    x
;;

let reify x =
  Pair.prj_exn Nat.prj_exn (List.prj_exn (Pair.prj_exn Nat.prj_exn reify_wheels)) x
;;

(************************************************************************************************)

let _ =
  Printf.printf "Relational version \n%!";
  let run n = run_r reify show n q qh in
  run
    1
    ( "test 1"
    , fun q -> fresh (a b) (q === pair a b) (FO.eval (from_int 3) (from_int 4) b a) );
  run
    1
    ( "test 2"
    , fun q -> fresh (a b) (q === pair a b) (FO.eval (from_int 3) (from_int 8) b a) )
;;
(* run
    1
    ( "test 3"
    , fun q -> fresh (a b) (q === pair a b) (FO.eval (from_int 6) (from_int 4) b a) );

  run
    1
    ( "test 3"
    , fun q -> fresh (a b) (q === pair a b) (FO.eval (from_int 5) (from_int 4) b a) ) *)
