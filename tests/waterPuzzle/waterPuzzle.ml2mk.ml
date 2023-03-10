(* Natural numbers in Peano encoding *)
open Peano

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

let ( - ) a b = if a >= b then a - b else 0

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
open HO

(*************************************************)

type set = Nat.ground * Nat.ground * Nat.ground * Nat.ground
[@@deriving gt ~options:{ show }]

type lset = Nat.logic * Nat.logic * Nat.logic * Nat.logic
[@@deriving gt ~options:{ show }]

type answer = move OCanren.Std.List.ground [@@deriving gt ~options:{ show }]

let set a b c d = pair a @@ pair b @@ pair c d

let _ =
  Printf.printf "%s\n"
  @@ show answer
  @@ Stdlib.List.hd
  @@ Stream.take ~n:1
  @@ run
       q
       (fun q ->
         fresh
           (a b c d)
           (c === nat 1 ||| (d === nat 1))
           (FO.eval (set (nat 5) (nat 3) (nat 0) (nat 0)) q (set a b c d)))
       (fun rr -> rr#reify (Std.List.prj_exn prj_exn))
;;]
