open List
open Peano

let list_map f xs =
  let rec helper xs =
    match xs with
    | [] -> []
    | x :: xs -> f x :: helper xs
  in
  helper xs
;;
