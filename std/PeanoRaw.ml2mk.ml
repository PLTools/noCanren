open OCanren.Std.Nat

let rec add n m =
  match n with
  | O -> m
  | S n' -> S (add n' m)
;;

let ( + ) = add

let rec mul n m =
  match n with
  | O -> O
  | S n' -> add m (mul n' m)
;;

let ( * ) = mul

let rec ge n = function
  | O -> true
  | S n' ->
    (match n with
     | O -> false
     | S n'' -> ge n'' n')
;;

let le n n' = ge n' n
let gt n n' = ge n n' && n <> n'
let lt n n' = gt n' n
let max x y = if ge x y then x else y
let min x y = if le x y then x else y
