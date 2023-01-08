type nat =
  | O
  | S of nat

let rec add a b =
  match a with
  | O -> b
  | S x -> add x (S b)
;;

type bottle =
  | Fst
  | Snd

type stepType =
  | Fill
  | Empty
  | Pour

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

let anotherBottle = function
  | Fst -> Snd
  | Snd -> Fst
;;

let createState bottle lvl1 lvl2 =
  match bottle with
  | Fst -> lvl1, lvl2
  | Snd -> lvl2, lvl1
;;

let checkStep (f, s) (t, b) capacities =
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
  match t with
  | Fill -> lvl1 = O
  | Empty -> lvl1 = capacities b
  | Pour ->
    let b' = anotherBottle b in
    not (lvl1 = O || lvl2 = capacities b')
;;

let doStep (f, s) (t, b) capacities =
  let lvl2 =
    match b with
    | Fst -> s
    | Snd -> f
  in
  match t with
  | Fill -> createState b (capacities b) lvl2
  | Empty -> createState b O lvl2
  | Pour ->
    let sum = add f s in
    let cap2 = capacities (anotherBottle b) in
    if greater sum cap2 then createState b (sub sum cap2) cap2 else createState b O sum
;;

let isFinishState (f, s) reqLvl = f = reqLvl || s = reqLvl

let checkAnswer answer capacities reqLvl =
  let rec checkAnswer state0 = function
    | [] -> isFinishState state0 reqLvl
    | x :: xs ->
      if checkStep state0 x capacities
      then checkAnswer (doStep state0 x capacities) xs
      else false
  in
  let startState = O, O in
  checkAnswer startState answer
;;

(****************************************************************************)

let capacities1 = function
  | Fst -> S (S (S (S O)))
  | Snd -> S (S (S (S (S (S (S (S (S O))))))))
;;
