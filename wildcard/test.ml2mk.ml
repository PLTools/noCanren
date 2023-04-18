let head_is_true = function
  | true :: _ -> true
  | _ -> false
;;

let rec forall = function
  | [] -> true
  | true :: xs -> forall xs
  | _ -> false
;;

let rec same_lens l1 l2 =
  match l1, l2 with
  | [], [] -> true
  | _ :: xs, _ :: ys -> same_lens xs ys
  | _ -> false
;;

let rec eq_lists l1 l2 =
  match l1, l2 with
  | [], [] -> true
  | x :: xs, y :: ys -> x = y && eq_lists xs ys
  | _ -> false
;;

let matching = function
  | [ _; true; false ] -> "1"
  | [ _; false; true ] -> "2"
  | [ true; _; _ ] -> "3"
  | [ false; false; _ ] -> "4"
  | [ _; _; true ] -> "5"
  | [ _; _; _ ] -> "6"
  | _ -> "7"
;;
