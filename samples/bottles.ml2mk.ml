open Peano

type bottle =
  | Fst
  | Snd

type stepType =
  | Fill
  | Empty
  | Pour

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
  | Fill -> lvl1 = 0
  | Empty -> lvl1 = capacities b
  | Pour ->
    let b' = anotherBottle b in
    not (lvl1 = 0 || lvl2 = capacities b')
;;

let doStep (f, s) (t, b) capacities =
  let lvl2 =
    match b with
    | Fst -> s
    | Snd -> f
  in
  match t with
  | Fill -> createState b (capacities b) lvl2
  | Empty -> createState b 0 lvl2
  | Pour ->
    let sum = f + s in
    let cap2 = capacities (anotherBottle b) in
    if sum > cap2 then createState b (sum - cap2) cap2 else createState b 0 sum
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
  let startState = 0, 0 in
  checkAnswer startState answer
;;

(****************************************************************************)

let capacities1 = function
  | Fst -> 4
  | Snd -> 9
;;
