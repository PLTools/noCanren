open OCanren.Std.Nat

let rec add n = function
  | O -> n
  | S n' -> S (add n n')
;;

let ( + ) = add

let rec mul n = function
  | O -> O
  | S n' -> add n (mul n n')
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
