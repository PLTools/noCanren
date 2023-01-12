type stick =
  | One
  | Two
  | Thr

let get name (s1, s2, s3) =
  match name with
  | One -> s1
  | Two -> s2
  | Thr -> s3
;;

let set name stack (s1, s2, s3) =
  match name with
  | One -> stack, s2, s3
  | Two -> s1, stack, s3
  | Thr -> s1, s2, stack
;;

let one_step (fromN, toN) state =
  match fromN <> toN with
  | false -> failwith "Ignore branch 1"
  | true ->
    (match get fromN state with
     | [] -> failwith "Ignore branch 2"
     | x :: xs ->
       (match get toN state with
        | [] -> set toN [ x ] (set fromN xs state)
        | y :: ys as r ->
          (match Peano.( < ) x y with
           | false -> failwith "Ignore branch 3"
           | true -> set toN (x :: r) (set fromN xs state))))
;;

let rec check state = function
  | [] -> get One state = [] && get Two state = []
  | x :: xs -> check (one_step x state) xs
;;

open Peano

let start_state = [ 0; 1; 2; 3; 4; 5 ], [], []
let answer = [ One, Thr; One, Two; Thr, Two; One, Thr; Two, One; Two, Thr; One, Thr ]

let answer' =
  [ One, Two
  ; One, Thr
  ; Two, Thr
  ; One, Two
  ; Thr, One
  ; Thr, Two
  ; One, Two
  ; One, Thr
  ; Two, Thr
  ; Two, One
  ; Thr, One
  ; Two, Thr
  ; One, Two
  ; One, Thr
  ; Two, Thr
  ]
;;