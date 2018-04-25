type num = O | S of num

let rec add a b =
  match a with
  | O   -> b
  | S x -> add x (S b)

let (|&|) a b = if a then b else false

let rec fibB n =
  match n with
  | O    -> true
  | S n0 ->
    match n0 with
    | O    -> true
    | S n1 -> (fibB n1) |&| (fibB n0)


let[@tabled] rec fib n =
  match n with
  | O    -> O
  | S n0 ->
    match n0 with
    | O    -> S O
    | S n1 -> add (fib n1) (fib n0)


let mul2 n = add n n

let a01 = S O
let a02 = mul2 a01
let a04 = mul2 a02
let a08 = mul2 a04
let a16 = mul2 a08
let a20 = add a16 a04
let a32 = mul2 a16
let a64 = mul2 a32

