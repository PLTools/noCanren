open Peano

let m1 = 1
let m2 = 2
let m3 = 5
let m4 = 10
let max_time = 17
let same_side (left_group, _) a b = List.mem a left_group = List.mem b left_group

let has_torch (left_group, torch_on_the_left) a =
  List.mem a left_group = torch_on_the_left
;;

let member x = x = m1 || x = m2 || x = m3 || x = m4

let can_move state = function
  | [ a ] -> member a && has_torch state a
  | [ a; b ] -> member a && member b && a < b && same_side state a b && has_torch state a
  | [] | _ :: _ :: _ :: _ -> false
;;

let move (left_group, torch_on_the_left) a =
  if List.mem a left_group
  then List.filter (( <> ) a) left_group, false
  else a :: left_group, true
;;

let verify moves =
  let[@tabled] rec verify_rec time ((left_group, torch_on_the_left) as state) = function
    | [] -> left_group = [] && (not torch_on_the_left) && time <= max_time
    | m :: moves ->
      time < max_time
      && can_move state m
      && verify_rec (time + List.fold_left max 0 m) (List.fold_left move state m) moves
  in
  verify_rec 0 ([ m1; m2; m3; m4 ], true) moves
;;

[@@@ocaml
  Printf.printf "Functional: %b\n"
  @@ verify [ [ 1; 2 ]; [ 2 ]; [ 5; 10 ]; [ 1 ]; [ 1; 2 ] ];
  Printf.printf "Functional: %b\n" @@ verify [ [ 1; 2 ]; [ 1 ]; [ 1; 5 ]; [ 1 ] ];
  Printf.printf "Functional: %b\n" @@ verify [ [ 1 ]; [ 2 ] ];
  Printf.printf "Functional: %b\n" @@ verify [ [ 1; 2; 5; 10 ] ]]
