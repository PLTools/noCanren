open OCanren
open OCanren.Std
open Tester
open List_test.FO

let show_nat n = Printf.sprintf "%d" @@ Nat.to_int n

let _ =
  let run n = run_r Nat.prj_exn show_nat n q qh in
  run (-1) ("lenght", is_5);
  run (-1) ("fold_left", is_10);
  run (-1) ("fold_right", is_15);
  run (-1) ("assoc", is_20);
  run (-1) ("assoc - failure", fail !!());
  run (-1) ("assoc backward", fun q -> fresh (l p) (key_value_list l) (assoc q l p))
;;

let _ =
  let run n = run_r (List.prj_exn Nat.prj_exn) (GT.show List.ground show_nat) n q qh in
  run 10 ("fold_left backward", fun q -> mul q (Peano.HO.from_int 10))
;;
