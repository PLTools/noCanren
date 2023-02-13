open Peano
open List

type set =
  { pin1 : nat list
  ; pin2 : nat list
  ; pin3 : nat list
  }

type pin =
  | A
  | B
  | C

let extra = function
  | A, B -> C
  | B, A -> C
  | A, C -> B
  | C, A -> B
  | B, C -> A
  | C, B -> A
  | A, A | B, B | C, C -> failwith "Incorrect pair of sticks"
;;

let select s = function
  | A -> s.pin1
  | B -> s.pin2
  | C -> s.pin3
;;

let permut move s =
  match move with
  | x, y -> { pin1 = select s x; pin2 = select s y; pin3 = select s (extra move) }
;;

let tumrep move s =
  match s with
  | { pin1 = x; pin2 = y; pin3 = z } ->
    (match move with
     | A, B -> { pin1 = x; pin2 = y; pin3 = z }
     | B, A -> { pin2 = x; pin1 = y; pin3 = z }
     | A, C -> { pin1 = x; pin3 = y; pin2 = z }
     | C, A -> { pin3 = x; pin1 = y; pin2 = z }
     | B, C -> { pin2 = x; pin3 = y; pin1 = z }
     | C, B -> { pin3 = x; pin2 = y; pin1 = z }
     | A, A | B, B | C, C -> failwith "Incorrect pair of sticks")
;;

let[@tabled] rec eval p s =
  match p with
  | [] -> s
  | move :: p' ->
    (match move with
     | x, y ->
       eval
         p'
         (if x = y
         then s
         else (
           match permut move s with
           | { pin1 = topA :: restA; pin2 = onB; pin3 = onC } ->
             tumrep
               move
               (match onB with
                | [] -> { pin1 = restA; pin2 = [ topA ]; pin3 = onC }
                | topB :: _ ->
                  (match topA < topB with
                   | true -> { pin1 = restA; pin2 = topA :: onB; pin3 = onC }
                   | false -> failwith "Incorrect step"))
           | { pin1 = []; _ } -> failwith "Incorrect state")))
;;
