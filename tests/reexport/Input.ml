type nonrec ('a, 'b) list =
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
