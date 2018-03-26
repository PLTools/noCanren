type nat = O | S of nat

type step = Left  of nat 
          | Right of nat 
          | Fill
          | Pour  of nat

type 'a maybe = Just of 'a
              | Nothing

type state = St of nat * nat * nat list

let rec (|+|) a b =
  match a with
  | O   -> b
  | S x -> x |+| (S b)


let rec (|>=|) a b =
  match a with
  | O   -> b = O
  | S x ->
    match b with
    | O   -> true
    | S y -> x |>=| y


let rec (|-|) a b =
  match b with
  | O   -> a
  | S y ->
    match a with
    | O   -> O
    | S x -> x |-| y


let (|&|) a b =
  if a then b else false

let rec elem l n =
  match l with
  | x::xs ->
    match n with
    | O   -> x
    | S m -> elem xs m 


let rec changeElem l n f =
  match l with
  | x::xs ->
    match n with
    | O   -> f x :: xs
    | S m -> x   :: changeElem xs m f    


let checkStep step state len cop =
  match state with
  | St (pos, fuel, sts) ->
    match step with
    | Left  d -> (pos |>=| d)           |&| (fuel |>=| d) |&| (d <> O)
    | Right d -> (len |>=| (pos |+| d)) |&| (fuel |>=| d) |&| (d <> O)
    | Pour  f -> (pos <> len) |&| (pos <> O) |&| (f <> O) |&| (fuel |>=| f)
    | Fill    ->
      match pos with
      | O   -> fuel <> cop
      | S x -> (fuel <> cop) |&| (elem sts x <> O)


let step step state len cop =
  match state with
  | St (pos, fuel, sts) ->
    match step with
    | Left  d -> St (pos |-| d, fuel |-| d, sts)
    | Right d -> St (pos |+| d, fuel |-| d, sts)
    | Pour  f -> (
      match pos with
      | S x -> St (pos, fuel |-| f, changeElem sts x (fun e -> f |+| e)))
    | Fill    ->
      match pos with
      | O   -> St (pos, cop, sts)
      | S x -> 
        let stationFuel = elem sts x in
        let totalFuel   = fuel |+| stationFuel in
        if totalFuel |>=| cop then St (pos, cop,       changeElem sts x (fun e -> stationFuel |-| cop))
                              else St (pos, totalFuel, changeElem sts x (fun e -> O))


let isFinishState state len =
  match state with
  | St (pos, fuel, sts) -> pos = len


let updateTotalFuel step state prevFuel cop =
  match step with
  | Left d  -> prevFuel
  | Right d -> prevFuel
  | Pour f  -> prevFuel
  | Fill    ->
    match state with
    | St (pos, fuel, sts) ->
      match pos with
      | O   -> (cop |+| prevFuel) |-| fuel
      | S x -> prevFuel


let isMove step =
  match step with
  | Left  x -> true
  | Right x -> true
  | Fill    -> false
  | Pour  x -> false


let checkAnswer answer len cop totalFuel =  
  let rec check state ans prevIsMove currentFuel =
    match ans with
    | []    -> (currentFuel = totalFuel) |&| isFinishState state len
    | x::xs ->
      let currIsMove = isMove x in
      if prevIsMove = currIsMove then false else
        if checkStep x state len cop then
          let newCF = updateTotalFuel x state currentFuel cop in
          if totalFuel |>=| newCF then check (step x state len cop) xs currIsMove newCF else false
        else false in

  let startState =
    let rec stations n =
      match n with
      | O   -> []
      | S m -> O :: stations m in
    St (O, cop, stations len) in

  check startState answer false cop


(****************************************************************************)

let one = S O
let two = S one
let thr = S two
let fou = S thr
let fiv = S fou
let six = S fiv
let sev = S six
let eig = S sev

let best = six |+| sev
let total = S (sev |+| sev)

let a4 = [Right one; Pour one; Left one; Fill; Right one; Fill; Right thr]

let answer = [Right two; Pour one; Left two;
              Fill; Right two; Pour one; Left two;
              Fill; Right two; Fill;     Right fiv]


let ans = [Right fiv]


let f x = checkAnswer x sev fiv best


let o = 1


