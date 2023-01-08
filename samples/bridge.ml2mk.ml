type peano =
  | O
  | S of peano

type person =
  | A
  | B
  | C
  | D

type step =
  | One of person
  | Two of person * person

let rec greater a0 b0 =
  match a0 with
  | O -> false
  | S x ->
    (match b0 with
     | O -> true
     | S y -> greater x y)
;;

let grForPerson x y =
  match x with
  | A ->
    (match y with
     | A -> false
     | B -> true
     | C -> true
     | D -> true)
  | B ->
    (match y with
     | A -> false
     | B -> false
     | C -> false
     | D -> true)
  | C ->
    (match y with
     | A -> false
     | B -> false
     | C -> false
     | D -> true)
  | D -> false
;;

let max a0 b0 = if greater a0 b0 then a0 else b0

let rec add a0 b0 =
  match a0 with
  | O -> b0
  | S x -> add x (S b0)
;;

let checkPerson (l, a0, b0, c0, d0) = function
  | A -> a0 = l
  | B -> b0 = l
  | C -> c0 = l
  | D -> d0 = l
;;

let checkStep state = function
  | One p -> checkPerson state p
  | Two (p, q) -> checkPerson state p && checkPerson state q && grForPerson p q
;;

let moveLight (l, a0, b0, c0, d0) = not l, a0, b0, c0, d0

let movePerson (l, a0, b0, c0, d0) = function
  | A -> l, not a0, b0, c0, d0
  | B -> l, a0, not b0, c0, d0
  | C -> l, a0, b0, not c0, d0
  | D -> l, a0, b0, c0, not d0
;;

let step state = function
  | One p -> moveLight (movePerson state p)
  | Two (p, q) -> moveLight (movePerson (movePerson state p) q)
;;

let getTime state times =
  match state with
  | One p -> times p
  | Two (p, q) -> max (times p) (times q)
;;

let getAnswer answer times =
  let start = true, true, true, true, true in
  let finish = false, false, false, false, false in
  let rec getAnswer answer state =
    match answer with
    | x :: xs ->
      if checkStep state x
      then (
        match getAnswer xs (step state x) [@heavy] with
        | None -> None
        | Some t1 -> Some (add (getTime x times) t1))
      else None
    | [] -> if state = finish then Some O else None
  in
  getAnswer answer start
;;

let standartTimes = function
  | A -> S O
  | B -> S (S O)
  | C -> S (S (S (S (S O))))
  | D -> S (S (S (S (S (S (S (S (S (S O)))))))))
;;
