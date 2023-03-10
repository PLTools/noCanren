open OCanren.Std.Nat

let rec add n m =
  match n with
  | O -> m
  | S n' -> S (add n' m)
;;

let rec sub n m =
  match n with
  | O ->
    (match m with
     | O -> O)
  | S n' ->
    (match m with
     | O -> n
     | S m' -> sub n' m')
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

let max x y =
  let rec max = function
    | O, S _ -> y
    | S _, O -> x
    | O, O -> x
    | S x, S y -> max (x, y)
  in
  max (x, y)
;;

let min x y =
  let rec min = function
    | O, S _ -> x
    | S _, O -> y
    | O, O -> x
    | S x, S y -> min (x, y)
  in
  min (x, y)
;;
