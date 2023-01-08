type person =
  | G
  | C
  | W
  | N

type state = St of bool * bool * bool * bool

let eqBool x y =
  match x with
  | true -> y
  | false -> not y
;;

let eqState x y =
  match x with
  | St (a1, a2, a3, a4) ->
    (match y with
     | St (b1, b2, b3, b4) -> eqBool a1 b2 && eqBool a2 b2 && eqBool a3 b3 && eqBool a4 b4)
;;

let checkState s =
  match s with
  | St (i0, g0, c0, w0) ->
    if eqBool i0 g0 then true else if eqBool i0 c0 then eqBool i0 w0 else false
;;

let checkStep state step =
  match state with
  | St (i0, g0, c0, w0) ->
    (match step with
     | N -> true
     | G -> eqBool i0 g0
     | C -> eqBool i0 c0
     | W -> eqBool i0 w0)
;;

let step s p =
  match s with
  | St (i0, g0, c0, w0) ->
    (match p with
     | G -> St (not i0, not g0, c0, w0)
     | C -> St (not i0, g0, not c0, w0)
     | W -> St (not i0, g0, c0, not w0)
     | N -> St (not i0, g0, c0, w0))
;;

let checkAnswer a =
  let startState = St (true, true, true, true) in
  let finishState = St (false, false, false, false) in
  let rec checkAnswer a state =
    match a with
    | [] -> eqState state finishState
    | x :: xs ->
      if checkStep state x
      then (
        let newState = step state x in
        if checkState newState then checkAnswer xs newState else false)
      else false
  in
  checkAnswer a startState
;;
