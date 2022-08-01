type nat =
  | O
  | S of nat

let rec add a b =
  match a with
  | S x -> add x (S b)
  | _ -> b
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
  | S x ->
    (match b with
     | S y -> greater x y
     | _ -> true)
  | _ -> false
;;

let rec sub a b =
  match b with
  | S y ->
    (match a with
     | S x -> sub x y
     | _ -> O)
  | _ -> a
;;

let anotherBottle = function
  | Fst -> Snd
  | _ -> Fst
;;

let createState bottle lvl1 lvl2 =
  match bottle with
  | Fst -> lvl1, lvl2
  | _ -> lvl2, lvl1
;;

let checkStep (f, s) (t, b) capacities =
  let lvl1 =
    match b with
    | Fst -> f
    | _ -> s
  in
  let lvl2 =
    match b with
    | Fst -> s
    | _ -> f
  in
  match t with
  | Fill -> lvl1 = O
  | Empty -> lvl1 = capacities b
  | _ ->
    let b' = anotherBottle b in
    not (lvl1 = O || lvl2 = capacities b')
;;

let doStep (f, s) (t, b) capacities =
  let lvl2 =
    match b with
    | Fst -> s
    | _ -> f
  in
  match t with
  | Fill -> createState b (capacities b) lvl2
  | Empty -> createState b O lvl2
  | _ ->
    let sum = add f s in
    let cap2 = capacities (anotherBottle b) in
    if greater sum cap2 then createState b (sub sum cap2) cap2 else createState b O sum
;;

let isFinishState (f, s) reqLvl = f = reqLvl || s = reqLvl

let checkAnswer answer capacities reqLvl =
  let rec checkAnswer state0 = function
    | x :: xs ->
      if checkStep state0 x capacities
      then checkAnswer (doStep state0 x capacities) xs
      else false
    | _ -> isFinishState state0 reqLvl
  in
  let startState = O, O in
  checkAnswer startState answer
;;

(****************************************************************************)

let capacities1 = function
  | Fst -> S (S (S (S O)))
  | _ -> S (S (S (S (S (S (S (S (S O))))))))
;;
