type num = Zero | Incr of num

type 'a _list = Empty | Cons of 'a * 'a _list

let rec map f l =
  match l with
  | Cons(x, xs) -> Cons(f x, map f xs)
  | Empty       -> Empty

let rec foldl f acc l =
  match l with
  | Cons(x, xs) -> foldl f (f acc x) xs
  | Empty       -> acc

let rec foldr f acc l =
  match l with
  | Cons(x, xs) -> f x (foldr f acc xs)
  | Empty       -> acc

let rec add x y =
  match x with
  | Incr a -> add a (Incr y)
  | Zero   -> y

let n1 = Zero
let n2 = Incr (Incr (Incr Zero))
let n3 = Incr Zero
let n4 = Incr (Incr Zero)

let l = Cons(n1, Cons(n2, Cons(n3, Cons(n4, Empty))))

let incr_list = map (fun x -> Incr x)

let sum = foldl add Zero

let res = sum (incr_list l)
