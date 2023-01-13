open Peano

let ( <&&> ) = (( && ) [@rel])

let rec reduce_wheel state wheel reduced_resource =
  match state with
  | [] -> failwith "Unepected wheel"
  | ((name, resource) as wheel_state) :: rest ->
    if name = wheel
    then (name, resource - reduced_resource) :: rest
    else wheel_state :: reduce_wheel rest wheel reduced_resource
;;

let do_step state (wheel1, wheel2, wheel3, wheel4) dist =
  let front_resource = 2 * dist in
  let back_resource = dist in
  reduce_wheel
    (reduce_wheel
       (reduce_wheel (reduce_wheel state wheel1 front_resource) wheel2 front_resource)
       wheel3
       back_resource)
    wheel4
    back_resource
;;

let is_correct_step (dist, (wheel1, wheel2, wheel3, wheel4)) state =
  dist
  <> 0
  <&&> (wheel1 <> wheel2)
  <&&> (wheel1 <> wheel3)
  <&&> (wheel1 <> wheel4)
  <&&> (wheel2 <> wheel3)
  <&&> (wheel2 <> wheel4)
  <&&> (wheel3 <> wheel4)
;;

let rec is_empty_state = function
  | [] -> true
  | (_, wheel) :: rest ->
    if wheel = 0 then is_empty_state rest else failwith "Non-empty wheel"
;;

let init_state resource amount =
  let rec init_state index amount =
    if amount = 0 then [] else (index, resource) :: init_state (index + 1) (amount - 1)
  in
  init_state 0 amount
;;

let eval init_resource wheels_amount steps =
  let rec eval state steps =
    match steps with
    | [] -> if is_empty_state state then 0 else failwith "Non-empty wheel"
    | ((dist, wheels) as step) :: rest ->
      if is_correct_step step state
      then dist + eval (do_step state wheels dist) rest
      else failwith "Incorrect step"
  in
  eval (init_state init_resource wheels_amount) steps
;;
