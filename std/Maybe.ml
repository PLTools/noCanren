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

let return_o x = x
let fail_o _ = failure
let let_star_bind_o x f res = fresh x0 (x x0) (f (( === ) x0) res)
let c60c38c38c62_o a b res = res === !!true &&& (a !!true &&& b !!true)
let c60c124c124c62_o a b res = res === !!true &&& (a !!true ||| b !!true)
let from_just_o x res = fresh x0 (res === x0) (x x0)
