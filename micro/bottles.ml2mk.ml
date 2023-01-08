type bottle =
  | Fst
  | Snd

type stepType =
  | Fill
  | Empty
  | Pour

type nat =
  | O
  | S of nat

let rec eqNat a b =
  match a with
  | O ->
    (match b with
     | O -> true
     | S _ -> false)
  | S x ->
    (match b with
     | O -> false
     | S y -> eqNat x y)
;;

let rec add a b =
  match a with
  | O -> b
  | S x -> add x (S b)
;;

let rec greater a b =
  match a with
  | O -> false
  | S x ->
    (match b with
     | O -> true
     | S y -> greater x y)
;;

let rec sub a b =
  match b with
  | O -> a
  | S y ->
    (match a with
     | O -> O
     | S x -> sub x y)
;;

let anotherBottle b =
  match b with
  | Fst -> Snd
  | Snd -> Fst
;;

let createState bottle lvl1 lvl2 =
  match bottle with
  | Fst -> lvl1, lvl2
  | Snd -> lvl2, lvl1
;;

let capacities b =
  match b with
  | Fst -> S (S (S (S O)))
  | Snd -> S (S (S (S (S (S (S (S (S O))))))))
;;

let checkStep state0 step0 =
  match state0 with
  | f, s ->
    (match step0 with
     | t, b ->
       let lvl1 =
         match b with
         | Fst -> f
         | Snd -> s
       in
       let lvl2 =
         match b with
         | Fst -> s
         | Snd -> f
       in
       (match t with
        | Fill -> eqNat lvl1 O
        | Empty -> eqNat lvl1 (capacities b)
        | Pour ->
          let b' = anotherBottle b in
          not (eqNat lvl1 O || eqNat lvl2 (capacities b'))))
;;

let doStep state0 step0 =
  match state0 with
  | f, s ->
    (match step0 with
     | t, b ->
       let lvl2 =
         match b with
         | Fst -> s
         | Snd -> f
       in
       (match t with
        | Fill -> createState b (capacities b) lvl2
        | Empty -> createState b O lvl2
        | Pour ->
          let sum = add f s in
          let cap2 = capacities (anotherBottle b) in
          if greater sum cap2
          then createState b (sub sum cap2) cap2
          else createState b O sum))
;;

let isFinishState state0 reqLvl =
  match state0 with
  | f, s -> eqNat f reqLvl || eqNat s reqLvl
;;

let checkAnswer answer reqLvl =
  let rec checkAnswer state0 answer =
    match answer with
    | [] -> isFinishState state0 reqLvl
    | x :: xs -> if checkStep state0 x then checkAnswer (doStep state0 x) xs else false
  in
  let startState = O, O in
  checkAnswer startState answer
;;
