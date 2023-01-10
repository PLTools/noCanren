open Test_nat

let rec add a b =
  match a with
  | O -> b
  | S a' -> S (add a' b)
;;
