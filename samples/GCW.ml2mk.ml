type person =
  | G
  | C
  | W
  | N

let checkState (i0, g0, c0, w0) =
  if i0 = g0 then true else if i0 = c0 then i0 = w0 else false
;;

let checkStep (i0, g0, c0, w0) = function
  | N -> true
  | G -> i0 = g0
  | C -> i0 = c0
  | W -> i0 = w0
;;

let step (i0, g0, c0, w0) = function
  | G -> not i0, not g0, c0, w0
  | C -> not i0, g0, not c0, w0
  | W -> not i0, g0, c0, not w0
  | N -> not i0, g0, c0, w0
;;

let checkAnswer a =
  let startState = true, true, true, true in
  let finishState = false, false, false, false in
  let rec checkAnswer a state =
    match a with
    | [] -> state = finishState
    | x :: xs ->
      if checkStep state x
      then (
        let newState = step state x in
        if checkState newState then checkAnswer xs newState else false)
      else false
  in
  checkAnswer a startState
;;
