type 'a maybe =
  | Nothing
  | Just of 'a

let return x = Just x
let fail = Nothing

let ( let* ) x f =
  match x with
  | Just a -> f a
  | Nothing -> Nothing
;;

let intersect f a b =
  let* a = a in
  let* b = b in
  return (f a b)
;;

let ( <&&> ) a b = intersect ( && ) a b

let union f a b =
  match a, b with
  | Just a, Just b -> return (f a b)
  | _, (Just _ as a) | (Just _ as a), _ -> a
  | _ -> fail
;;

let ( <||> ) a b = union ( || ) a b

let from_just = function
  | Just x -> x
  | Nothing -> failwith "Argument of 'from_just' is 'Nothing'."
;;

open OCanren

module HO = struct
  let return x = x
  let fail _ = failure
  let let_star_bind x f res = fresh x0 (x x0) (f (( === ) x0) res)
  let ( <&&> ) a b res = res === !!true &&& (a !!true &&& b !!true)
  let ( <||> ) a b res = res === !!true &&& (a !!true ||| b !!true)
  let from_just x res = fresh x0 (res === x0) (x x0)
end

module FO = struct
  let return x y = x === y
  let fail _ = failure
  let let_star_bind x f res = f x res
  let ( <&&> ) a b res = res === !!true &&& (a === !!true &&& (b === !!true))
  let ( <||> ) a b res = res === !!true &&& (a === !!true ||| (b === !!true))
  let from_just x res = x === res
end