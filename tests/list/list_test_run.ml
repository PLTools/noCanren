open OCanren
open OCanren.Std
open Tester
open List_test.FO

let show_nat n = Printf.sprintf "%d" @@ Nat.to_int n

let show_lnat n =
  let rec helper n =
    match n with
    | Value Nat.O -> 0, None
    | Value (Nat.S n) ->
      let x, v = helper n in
      x + 1, v
    | Var _ -> 0, Some n
  in
  match helper n with
  | 0, None -> "0"
  | n, None -> Printf.sprintf "%d" n
  | 0, Some v -> GT.show Nat.logic v
  | n, Some v -> Printf.sprintf "%d + %s" n (GT.show Nat.logic v)
;;

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

let _ =
  let run n = run_r (List.reify Nat.reify) (GT.show List.logic show_lnat) n qr qrh in
  run 10 ("fold_right2 backward", fun q r -> dot_product q r (Peano.HO.from_int 10))
;;

let _ =
  let run n = run_r (List.reify Nat.reify) (GT.show List.logic show_lnat) n q qh in
  run 10 ("mapi backward", fun q -> dot_product_with_indexes q (Peano.HO.from_int 10))
;;
