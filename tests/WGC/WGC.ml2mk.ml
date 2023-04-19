open List
open Maybe

type move =
  | Empty
  | Goat
  | Wolf
  | Cabbage

type solution = move list

type side =
  { man : bool
  ; goat : bool
  ; cabbage : bool
  ; wolf : bool
  }

type state = side * side

let safe (left, right) =
  let safe_side side =
    side.man
    || (not side.goat)
    || (side.goat && ((not side.cabbage) || (side.cabbage && side.wolf)))
  in
  left.man <> right.man
  && left.goat <> right.goat
  && left.cabbage <> right.cabbage
  && left.wolf <> right.wolf
  && safe_side left
  && safe_side right
;;

let step mov (left, right) =
  let swap state =
    let* left, right = state in
    return (right, left)
  in
  let move dep arr =
    match mov with
    | Empty -> Just ({ dep with man = false }, { arr with man = true })
    | Goat ->
      if dep.goat
      then
        Just ({ dep with man = false; goat = false }, { arr with man = true; goat = true })
      else Nothing
    | Wolf ->
      if dep.wolf
      then
        Just ({ dep with man = false; wolf = false }, { arr with man = true; wolf = true })
      else Nothing
    | Cabbage ->
      if dep.cabbage
      then
        Just
          ( { dep with man = false; cabbage = false }
          , { arr with man = true; cabbage = true } )
      else Nothing
  in
  if left.man then move left right else swap (move right left)
;;

let rec eval init moves =
  let* final =
    List.fold_left
      (fun state move ->
        let* state' = state in
        if safe state' then step move state' else Nothing)
      (Just init)
      moves
  in
  if safe final then Just final else Nothing
;;
