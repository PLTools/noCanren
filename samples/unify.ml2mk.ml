open List

type nat =
  | O
  | S of nat

type term =
  | Var of nat
  | Constr of nat * term list

let rec get_term var subst =
  match subst with
  | x :: xs ->
    (match var with
     | O -> x
     | S n -> get_term n xs)
;;

let rec forall2 f l1 l2 =
  match l1, l2 with
  | [], [] -> true
  | x :: xs, y :: ys -> f x y && forall2 f xs ys
;;

let rec check_uni_f subst ft t =
  match ft, t with
  | Constr (n1, a1), Constr (n2, a2) -> n1 = n2 && forall2 (check_uni_f subst) a1 a2
  | _, Var v -> ft = get_term v subst
;;

let rec check_uni subst t1 t2 =
  match t1, t2 with
  | Constr (n1, a1), Constr (n2, a2) -> n1 = n2 && forall2 (check_uni subst) a1 a2
  | Var v, Constr (n, a) -> check_uni_f subst (get_term v subst) t2
  | Constr (n, a), Var v -> check_uni_f subst (get_term v subst) t1
  | Var v1, Var v2 -> get_term v1 subst = get_term v2 subst
;;

(*
get_term(o   , [X|XS], X).
get_term(s(A), [X|XS], Y) :- get_term(A, XS, Y).

for_args_f([]    , []    , S).
for_args_f([X|XS], [Y|YS], S) :- check_uni_f(X, Y, S), for_args_f(XS, YS, S).

check_uni_f(T       , v(V)    , S) :- get_term(V, S, T).
check_uni_f(c(N, A1), c(N, A2), S) :- for_args_f(A1, A2, S).

for_args([]    , []    , S).
for_args([X|XS], [Y|YS], S) :- check_uni(X, Y, S), for_args(XS, YS, S).

check_uni(v(V1)   , v(V2)   , S) :- get_term(V1, S, T), get_term(V2, S, T).
check_uni(v(V)    , c(N, A) , S) :- get_term(V, S, T), check_uni_f(T, c(N, A), S).
check_uni(c(N, A) , v(V)    , S) :- get_term(V, S, T), check_uni_f(T, c(N, A), S).
check_uni(c(N, A1), c(N, A2), S) :- for_args(A1, A2, S).


:- check_uni(
  c(appendo, [
    c(cons, [c(aa,[]), c(cons, [c(bb,[]), c(nil, [])])]),
    c(cons, [c(cc,[]), c(cons, [c(dd,[]), c(nil, [])])]),
    v(o)]),
  c(appendo, [
    c(cons, [v(s(o)), v(s(s(o)))]),
    v(s(s(s(o)))),
    c(cons, [v(s(o)), v(s(s(s(s(o)))))])]),
  S)
 *)

(* let eq_nat a b = a = b

let eq_term a b = a = b

let rec get_term var subst =
  match subst with
  | x::xs ->
    match var with
    | O   -> x
    | S n -> get_term n xs

let rec check_uni_semifree t1 t2 subst =
  let rec all_check_uni_semifree l1 l2 subst =
    match l1, l2 with
    | []   , []    -> true
    | x::xs, y::ys -> check_uni_semifree x y subst && all_check_uni_semifree xs ys subst in

  match t1, t2 with
  | Constr (n1, a1), Constr (n2, a2) ->
    begin match eq_nat n1 n2 with
    | true -> all_check_uni_semifree a1 a2 subst
    end
  | _, Var v ->
    eq_term (get_term v subst) t1

let rec check_uni t1 t2 subst =
  let rec all_check_uni l1 l2 subst =
    match l1, l2 with
    | []   , []    -> true
    | x::xs, y::ys -> check_uni x y subst && all_check_uni xs ys subst in

  match t1, t2 with
  | Constr (n1, a1), Constr (n2, a2) ->
    begin match eq_nat n1 n2 with
    | true -> all_check_uni a1 a2 subst
    end
  | Var v, Constr (n, a) ->
    check_uni_semifree (get_term v subst) t2 subst
  | Constr (n, a), Var v ->
    check_uni_semifree (get_term v subst) t1 subst
  | Var v1, Var v2 ->
    eq_term (get_term v1 subst) (get_term v2 subst) *)
