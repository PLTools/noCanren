type 'a list = Nil | Cons of 'a * 'a list

let rec foldl f (init: 'acc -> 'acc) xs = match xs with
  | Nil -> init
  | Cons(x,xs) -> foldl f (f init x) xs

let foldr f init xs = foldl (fun acc x -> fun r -> acc (f x r) ) (fun x -> x) xs init

