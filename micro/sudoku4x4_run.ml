open OCanren
open OCanren.Std
open Tester
open Sudoku4x4

(*************************************************)

let show_num = function
  | N1 -> "1"
  | N2 -> "2"
  | N3 -> "3"
  | N4 -> "4"
;;

let show_sudoku4x4 = function
  | S4x4 (a11, a12, a13, a14, a21, a22, a23, a24, a31, a32, a33, a34, a41, a42, a43, a44)
    ->
    let s = show_num in
    let s1 = Printf.sprintf "%s %s | %s %s" (s a11) (s a12) (s a13) (s a14) in
    let s2 = Printf.sprintf "%s %s | %s %s" (s a21) (s a22) (s a23) (s a24) in
    let s3 = Printf.sprintf "----+----" in
    let s4 = Printf.sprintf "%s %s | %s %s" (s a31) (s a32) (s a33) (s a34) in
    let s5 = Printf.sprintf "%s %s | %s %s" (s a41) (s a42) (s a43) (s a44) in
    Printf.sprintf "%s\n%s\n%s\n%s\n%s" s1 s2 s3 s4 s5
;;

let myshow = show_sudoku4x4

(*************************************************)

(** For high order conversion **)
let check_sudoku q r = check_sudoku (( === ) q) r

(** For call-by-need conversion **)
(* let check_sudoku q r = (snd check_sudoku) ([Obj.magic q], (===) q) r *)

let _ = run_exn myshow 1 q qh ("sudoku", fun q -> check_sudoku q !!true)
