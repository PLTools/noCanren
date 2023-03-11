(* Natural numbers in Peano encoding *)
open Peano

let ( - ) a b = if a >= b then a - b else 0

(* A possible moves:
   --- pour A to B
   --- pour B to A
   --- fill A
   --- fill B
*)
type move =
  | AB
  | BA
  | FillA
  | FillB

(*
Make this type definition compile

type set = (nat, nat, nat, nat)
*)

(*
    A set is a quadruple:

       (capacity of A, capacity of B, contents of A, contents of B)
*)

(* set -> move -> set

   Performs a given move for a given set if possible
*)

let step (capA, capB, a, b) = function
  | FillA -> capA, capB, capA, b
  | FillB -> capA, capB, a, capB
  | AB ->
    let diff = capB - b in
    capA, capB, a - diff, b - min diff a
  | BA ->
    let diff = capA - a in
    capA, capB, min diff b + a, b - diff
;;

(* set -> move list -> set

   Performs a number of moves for a given set
*)
let rec eval set = function
  | [] -> set
  | m :: moves -> eval (step set m) moves
;;

[@@@ocaml
open GT
open OCanren
open OCanren.Std
open HO;;

(*************************************************)

@type set = Nat.ground * Nat.ground * Nat.ground * Nat.ground with show;;

@type lset = Nat.logic * Nat.logic * Nat.logic * Nat.logic with  show;;

@type answer = move OCanren.Std.List.ground with show

let _ =
  Printf.printf "%s\n"
  @@ show answer
  @@ Stdlib.List.hd
  @@ Stream.take ~n:1
  @@ run
       q
       (fun q -> ocanren {
         fresh a, b, c, d in
           FO.eval (5, 3, 0, 0) q (a, b, c, d) &
           {a == 1 | d == 1}})
       (fun rr -> rr#reify (Std.List.prj_exn prj_exn))
;;]
