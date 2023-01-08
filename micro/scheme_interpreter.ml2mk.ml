type variable =
  | First
  | Next of variable

type identifier =
  | Lambda
  | Quote
  | List
  | Var of variable

type term =
  | Ident of identifier
  | Seq of term list

type result =
  | Val of term
  | Closure of identifier * term * (identifier * result) list

let rec eq_var a b =
  match a with
  | First ->
    (match b with
     | First -> true
     | Next _ -> false)
  | Next x ->
    (match b with
     | First -> false
     | Next y -> eq_var x y)
;;

let eq_id a b =
  match a with
  | Lambda ->
    (match b with
     | Lambda -> true
     | Quote -> false
     | List -> false
     | Var _ -> false)
  | Quote ->
    (match b with
     | Lambda -> false
     | Quote -> true
     | List -> false
     | Var _ -> false)
  | List ->
    (match b with
     | Lambda -> false
     | Quote -> false
     | List -> true
     | Var _ -> false)
  | Var x ->
    (match b with
     | Lambda -> false
     | Quote -> false
     | List -> false
     | Var y -> eq_var x y)
;;

let rec lookup x env =
  match env with
  | (y, res) :: env' -> if eq_id x y then res else lookup x env'
;;

let rec not_in_env x env =
  match env with
  | [] -> true
  | (y, res) :: env' -> if eq_id x y then false else not_in_env x env'
;;

let rec eval term env =
  let lambda_handler ts env =
    match not_in_env Lambda env with
    | true ->
      (match ts with
       | [ Seq [ Ident i ]; body ] -> Closure (i, body, env))
  in
  let quote_handler ts env =
    match not_in_env Quote env with
    | true ->
      (match ts with
       | [ t ] -> Val t)
  in
  let list_handler ts env =
    match not_in_env List env with
    | true ->
      let eval_val t =
        match eval t env with
        | Val v -> v
      in
      let rec map_eval_val l =
        match l with
        | x :: xs -> eval_val x :: map_eval_val xs
        | [] -> []
      in
      Val (Seq (map_eval_val ts))
  in
  match term with
  | Ident x -> lookup x env
  | Seq (t :: ts) ->
    (match t with
     | Ident id ->
       (match id with
        | Lambda -> lambda_handler ts env
        | Quote -> quote_handler ts env
        | List -> list_handler ts env)
     | Seq s ->
       (match ts with
        | [ arg ] ->
          (match eval t env with
           | Closure (x, body, env') -> eval body ((x, eval arg env) :: env'))))
;;
