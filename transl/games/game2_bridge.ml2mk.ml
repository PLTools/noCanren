type peano  = O | S of peano
type person = A | B | C | D
type step   = One of person | Two of person * person
type 'a opt = Nothing | Just of 'a
type state = St of bool * bool * bool * bool * bool


let rec greater a0 b0 =
  match a0 with
  | O   -> false
  | S x ->
    match b0 with
    | O  -> true
    | S y -> greater x y

let grForPerson x y =
  match x with
  | A -> (match y with | A -> false | B -> true  | C -> true  | D -> true)
  | B -> (match y with | A -> false | B -> false | C -> false | D -> true)
  | C -> (match y with | A -> false | B -> false | C -> false | D -> true)
  | D -> false

let max a0 b0 =
  if greater a0 b0 then a0 else b0


let rec add a0 b0 =
  match a0 with
  | O   -> b0
  | S x -> add x (S b0)


let conj a0 b0 =
  if a0 then b0 else false

let not a0 =
  if a0 then false else true


let checkPerson state person =
  match state with
  | St (l, a0, b0, c0, d0) ->
    match person with
    | A -> a0 = l
    | B -> b0 = l
    | C -> c0 = l
    | D -> d0 = l


let checkStep state step =
  match step with
  | One p      -> checkPerson state p
  | Two (p, q) -> conj (conj (checkPerson state p) (checkPerson state q)) (grForPerson p q)


let moveLight state =
  match state with
  | St (l, a0, b0, c0, d0) -> St (not l, a0, b0, c0, d0) 


let movePerson state person =
  match state with
  | St (l, a0, b0, c0, d0) ->
    match person with
    | A -> St (l, not a0, b0, c0, d0)
    | B -> St (l, a0, not b0, c0, d0)
    | C -> St (l, a0, b0, not c0, d0)
    | D -> St (l, a0, b0, c0, not d0)


let step state step =
  match step with
  | One p      -> moveLight (movePerson state p)
  | Two (p, q) -> moveLight (movePerson (movePerson state p) q)

let getTime state times =
  match state with
  | One p      -> times p
  | Two (p, q) -> max (times p) (times q)


let getAnswer answer times time =
  let start  = St (true, true, true, true, true) in
  let finish = St (false, false, false, false, false) in

  let rec getAnswer answer state num =
    if greater num time then false else
      match answer with
      | x :: xs ->
        if checkStep state x then
          let newNum   = add (getTime x times) num in
          let newState = step state x in
          getAnswer xs newState newNum
        else false
      | [] -> if state = finish then num = time else false in

  getAnswer answer start O
        

let standartTimes p = 
  match p with
  | A -> S O
  | B -> S (S O)
  | C -> S (S (S (S (S O))))
  | D -> S (S (S (S (S (S (S (S (S (S O)))))))))

let a17 = S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S O))))))))))))))))

let a19 = S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S O))))))))))))))))))

let a = [1;2;3]

(****************************************************************************)

(*
let rec numToInt n =
  match n with
  | O   -> 0
  | S x -> 1 + numToInt x

let rec list2mylist = function
  | []    -> Nil
  | x::xs -> Cons (x, list2mylist xs)


let badAnswer  = list2mylist [Two(A, B); One C]
let weakAnswer = list2mylist [Two(A, B); Two(A, B); Two(A, B); One A; Two(A, C); One A; Two(A, D)]
let goodAnswer = list2mylist [Two(A, B); One A; Two(A, C); One A; Two(A, D)]
let bestAnswer = list2mylist [Two(A, B); One A; Two(C, D); One B; Two(A, B)]



let print a =
  match getAnswer a standartTimes with
  | Nothing -> print_string "Baaaaad!\n"
  | Just n  -> print_int (numToInt n);
               print_string "\n"


let main = print badAnswer;
           print weakAnswer;
           print goodAnswer;
           print bestAnswer

*)
