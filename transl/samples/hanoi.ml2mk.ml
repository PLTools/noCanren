type nat = Z | S of nat

type stick = One | Two | Thr

type 'a triple = Triple of 'a * 'a * 'a

(**********************************************)

(* let rec nat2int = function
  | Z   -> 0
  | S x -> 1 + nat2int x

let rec int2nat n =
  if n = 0 then Z else S (int2nat @@ n - 1)

let print_state state =
  let l1 = List.map nat2int @@ state One in
  let l2 = List.map nat2int @@ state Two in
  let l3 = List.map nat2int @@ state Thr in
  Printf.printf "--------------\n";
  List.iter (Printf.printf "%d ") @@ List.rev l1;
  Printf.printf "\n--------------\n";
  List.iter (Printf.printf "%d ") @@ List.rev l2;
  Printf.printf "\n--------------\n";
  List.iter (Printf.printf "%d ") @@ List.rev l3;
  Printf.printf "\n--------------\n" *)

(**********************************************)

let rec less a b =
  match b with
  | S b' -> match a with
           | Z    -> true
           | S a' -> less a' b'

let get name state =
  match state with
  | Triple (s1, s2, s3) ->
    match name with
    | One -> s1
    | Two -> s2
    | Thr -> s3

let set name stack state =
  match state with
  | Triple (s1, s2, s3) ->
    match name with
    | One -> Triple (stack, s2, s3)
    | Two -> Triple (s1, stack, s3)
    | Thr -> Triple (s1, s2, stack)

let one_step step state =
  match step with
  | fromN, toN ->
    match fromN <> toN with
    | true ->
      match get fromN state with
      | x :: xs ->
        match get toN state with
        | []      -> set toN [x] (set fromN xs state)
        | y :: ys ->
          match less x y with
          | true ->
            set toN (x :: y :: ys) (set fromN xs state)


let rec check state steps =
  match steps with
  | []      -> get One state = [] && get Two state = []
  | x :: xs -> check (one_step x state) xs

let start_state = Triple ([Z; S Z; S (S Z); S (S (S Z)); S (S (S (S Z))); S (S (S (S (S Z))))], [], [])

let answer = [One, Thr; One, Two; Thr, Two; One, Thr; Two, One; Two, Thr; One, Thr]

let answer' = [One, Two; One, Thr; Two, Thr; One, Two; Thr, One; Thr, Two; One, Two; One, Thr; Two, Thr; Two, One; Thr, One; Two, Thr; One, Two; One, Thr; Two, Thr]

(* let print_bool b = Printf.printf (if b then "yes" else "no")

let _ =
  print_bool (check start_state answer) *)
