type nat =
  | O
  | S of nat

let rec add a b =
  match a with
  | O -> b
  | S x -> add x (S b)
;;
