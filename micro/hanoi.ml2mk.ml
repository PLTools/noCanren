type nat =
  | Z
  | S of nat

type stick =
  | One
  | Two
  | Thr

type 'a triple = Triple of 'a * 'a * 'a

let notEqStick x y =
  match x with
  | One ->
    (match y with
     | One -> false
     | Two -> true
     | Thr -> true)
  | Two ->
    (match y with
     | One -> true
     | Two -> false
     | Thr -> true)
  | Thr ->
    (match y with
     | One -> true
     | Two -> true
     | Thr -> false)
;;

let isNil l =
  match l with
  | [] -> true
  | _ :: _ -> false
;;

let rec less a b =
  match b with
  | S b' ->
    (match a with
     | Z -> true
     | S a' -> less a' b')
;;

let get name state =
  match state with
  | Triple (s1, s2, s3) ->
    (match name with
     | One -> s1
     | Two -> s2
     | Thr -> s3)
;;

let set name stack state =
  match state with
  | Triple (s1, s2, s3) ->
    (match name with
     | One -> Triple (stack, s2, s3)
     | Two -> Triple (s1, stack, s3)
     | Thr -> Triple (s1, s2, stack))
;;

let one_step step state =
  match step with
  | fromN, toN ->
    (match notEqStick fromN toN with
     | true ->
       (match get fromN state with
        | x :: xs ->
          (match get toN state with
           | [] -> set toN [ x ] (set fromN xs state)
           | y :: ys ->
             (match less x y with
              | true -> set toN (x :: y :: ys) (set fromN xs state)))))
;;

let rec check state steps =
  match steps with
  | [] -> isNil (get One state) && isNil (get Two state)
  | x :: xs -> check (one_step x state) xs
;;
