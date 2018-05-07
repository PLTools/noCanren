type bottle = Fst | Snd
type stepType = Fill | Empty | Pour
type nat = O | S of nat
type step = Step of stepType * bottle
type state = State of nat * nat


let rec add a b =
  match a with
  | O   -> b
  | S x -> add x (S b)


let rec greater a b =
  match a with
  | O   -> false
  | S x ->
    match b with
    | O   -> true
    | S y -> greater x y


let rec sub a b =
  match b with
  | O   -> a
  | S y ->
    match a with
    | O   -> O
    | S x -> sub x y


let not b =
  if b then false else true


let anotherBottle b =
  match b with
  | Fst -> Snd
  | Snd -> Fst


let createState bottle lvl1 lvl2 =
   match bottle with
   | Fst -> State (lvl1, lvl2)
   | Snd -> State (lvl2, lvl1)


let checkStep state0 step0 capacities =
  match state0 with
  | State (f, s) ->
    match step0 with
    | Step (t, b) ->
      let lvl1 = match b with | Fst -> f | Snd -> s in
      let lvl2 = match b with | Fst -> s | Snd -> f in
        match t with
        | Fill  -> lvl1 = O
        | Empty -> lvl1 = capacities b
        | Pour  ->
          let b'      = anotherBottle b in
          not (lvl1 = O || lvl2 = capacities b')


let doStep state0 step0 capacities =
  match state0 with
  | State (f, s) ->
    match step0 with
    | Step (t, b) ->
      let lvl2 = match b with | Fst -> s | Snd -> f in
        match t with
       | Fill  -> createState b (capacities b) lvl2
       | Empty -> createState b O lvl2
       | Pour  ->
         let sum  = add f s in
         let cap2 = capacities (anotherBottle b) in
         if greater sum cap2 then createState b (sub sum cap2) cap2
         else createState b O sum


let isFinishState state0 reqLvl =
  match state0 with
  | State (f, s) -> f = reqLvl || s = reqLvl


let checkAnswer answer capacities reqLvl =
  let rec checkAnswer state0 answer =
    match answer with
    | []      -> isFinishState state0 reqLvl
    | x :: xs ->
      if checkStep state0 x capacities then
        checkAnswer (doStep state0 x capacities) xs
      else false in

   let startState = State (O, O) in
   checkAnswer startState answer


(****************************************************************************)

let rec length l =
  match l with
  | []      -> O
  | x :: xs -> S (length xs)

let a8 = S(S(S(S(S(S(S(S O)))))))


let capacities1 b =
  match b with
  | Fst -> S (S (S (S O)))
  | Snd -> S (S (S (S (S (S (S (S (S O))))))))


let reqLvl1 = S (S (S (S (S (S O)))))

let answer = [ Step (Fill, Fst);   Step (Pour, Fst);  Step (Fill, Fst);   Step (Pour, Fst);
               Step (Fill, Fst);   Step (Pour, Fst);  Step (Empty, Snd);  Step (Pour, Fst);
               Step (Fill, Fst);   Step (Pour, Fst);  Step (Fill, Fst);   Step (Pour, Fst);
               Step (Empty, Snd);  Step (Pour, Fst);  Step (Fill, Fst);   Step (Pour, Fst)]

let badAns =  Step (Empty, Fst) :: answer
