type number = O | S of number

let incr = fun x -> S x

let rec add a b =
  match a with
  | O   -> b
  | S x -> add x (incr b)

let rec mul a b =
  match a with
  | O   -> O
  | S x -> match x with
           | O   -> b
           | S y -> add b (mul x b)
