type person = G | C | W | N
type state = St of bool * bool * bool * bool


let checkState s =
  match s with
  | St (i0, g0, c0, w0) ->
    if i0 = g0 then true else
      if i0 = c0 then i0 = w0 else false

let checkStep state step =
  match state with
  | St (i0, g0, c0, w0) ->
    match step with
    | N -> true
    | G -> i0 = g0
    | C -> i0 = c0
    | W -> i0 = w0


let step s p =
  match s with
  | St (i0, g0, c0, w0) ->
    match p with
    | G -> St (not i0, not g0, c0, w0)
    | C -> St (not i0, g0, not c0, w0)
    | W -> St (not i0, g0, c0, not w0)
    | N -> St (not i0, g0, c0, w0)


let checkAnswer a =
  let startState  = St (true, true, true, true) in
  let finishState = St (false, false, false, false) in
  let rec checkAnswer a state =
    match a with
    | []    -> state = finishState
    | x::xs ->
      if checkStep state x then
        let newState = step state x in
          if checkState newState then checkAnswer xs newState else false
      else false in

  checkAnswer a startState
