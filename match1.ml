let f x y =
  match (x,y) with
  | [],_ -> 1
  | _,[] -> 2
  | x::xs,y::ys -> 3
