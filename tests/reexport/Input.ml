type nonrec ('a, 'b) mylist =
  | Nil
  | Cons of 'a * 'b

type nonrec 'a xxx =
  | A
  | B
  | C

let good_a a =
  match a with
  | A | B -> true
  | C -> false
;;

let good_list xs =
  match xs with
  | Cons (A, Cons (B, Nil)) | Cons (B, Cons (A, Nil)) -> true
;;

type nat =
  | Z
  | S of nat

let rec even_nat n =
  match n with
  | Z -> true
  | S (S p) -> even_nat p
  | S Z -> false
;;

type nonrec id =
  | A
  | B

type nonrec rel =
  | Sub of id * id
  | Descr of id * id
