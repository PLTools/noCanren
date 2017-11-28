type 'a my_list =
 | Nil
 | Cons of 'a * 'a my_list

(*********************************************************************************)

type 'a maybe =
 | Nothing
 | Just of 'a

(*********************************************************************************)

type num =
 | Z
 | S of num

(*********************************************************************************)

type ('a, 'b) pair =
 | Pair of 'a * 'b

(*********************************************************************************)

type ('a, 'b, 'c) tuple =
 | Tuple of 'a * 'b * 'c

(*********************************************************************************)

type 'a lambda_type =
 | TInt
 | TBool
 | TVar of 'a
 | TFun of 'a lambda_type * 'a lambda_type

(*********************************************************************************)

type 'a gen_type =
 | Gen of 'a my_list * 'a lambda_type

(*********************************************************************************)

type literal =
 | LInt of int
 | LBool of bool

(*********************************************************************************)

type 'a lambda =
 | Var  of 'a
 | Lit  of literal
 | App  of 'a lambda * 'a lambda
 | Abst of 'a * 'a lambda
 | Let  of 'a * 'a lambda * 'a lambda

(*********************************************************************************)
(*********************************************************************************)
(*********************************************************************************)
(*
let rec remove e l =
  match l with
  | Nil           -> Nil
  | Cons (e', l') ->
    match e = e' with
    | true  -> remove e l'
    | false -> Cons (e', remove e l')

(*********************************************************************************)

let rec remove_list rem l =
  match rem with
  | Nil            -> l
  | Cons (x, rem') -> remove_list rem' (remove x l)

(*********************************************************************************)

let rec remove_repeats l =
  match l with
  | Nil           -> Nil
  | Cons (e', l') -> Cons (e', remove_repeats (remove e' l'))

(*********************************************************************************)

let rec append a b =
  match a with
  | Nil          -> b
  | Cons (x, a') -> Cons(x, append a' b)

(*********************************************************************************)

let free_vars typ =
  let rec fvars typ =
    match typ with
    | TInt        -> Nil
    | TBool       -> Nil
    | TVar v      -> Cons(v, Nil)
    | TFun (a, b) -> append (fvars a) (fvars b) in
  remove_repeats (fvars typ)

(*********************************************************************************)

let gen_free_vars gtyp =
  match gtyp with
  | Gen (vars, t) -> remove_list vars (free_vars t)

(*********************************************************************************)

let env_free_vars env =
  let rec get_fvs env =
    match env with
    | Nil            -> Nil
    | Cons (p, env') ->
      match p with
      | Pair(v, gtyp) -> append (gen_free_vars gtyp) (get_fvs env') in
  remove_repeats (get_fvs env)

(*********************************************************************************)
(*********************************************************************************)

let rec lookup v s =
  match s with
  | Nil          -> Nothing
  | Cons (p, xs) ->
    match p with
    | Pair (v', t) ->
      match v = v' with
      | true  -> Just t
      | false -> lookup v xs

(*********************************************************************************)

let rec delete e l =
  match l with
  | Nil          -> l
  | Cons (p, l') ->
    match p with
    | Pair (e', t) ->
      match e = e' with
      | false -> Cons (p, delete e l')
      | true  -> delete e l'

(*********************************************************************************)

let rec delete_list del l =
  match del with
  | Nil           -> l
  | Cons(e, del') -> delete_list del' (delete e l)

(*********************************************************************************)

let rec map f l =
  match l with
  | Nil          -> Nil
  | Cons (x, l') -> Cons (f x, map f l')

(*********************************************************************************)

let mb_bind a f =
  match a with
  | Nothing -> Nothing
  | Just x  -> f x

(*********************************************************************************)

let rec apply s typ =
  match typ with
  | TInt        -> typ
  | TBool       -> typ
  | TFun (a, b) -> TFun (apply s a, apply s b)
  | TVar v      ->
    match lookup v s with
    | Nothing   -> typ
    | Just typ' -> typ'

(*********************************************************************************)

let gen_apply s gtyp =
  match gtyp with
  | Gen (vars, typ) -> Gen (vars, apply (delete_list vars s) typ)

(*********************************************************************************)

let env_apply s env =
  let member_apply p =
    match p with
    | Pair (v, gtyp) -> Pair (v, gen_apply s gtyp) in
  map member_apply env

(*********************************************************************************)
(*********************************************************************************)

let compose_subst s1 s2 =
  let apply_member p =
    match p with
    | Pair (k, v) -> Pair(k, apply s1 v) in
  append (map apply_member s2) s1

(*********************************************************************************)

let generalize env typ =
  let vars = remove_list (env_free_vars env) (free_vars typ) in
  Gen (vars, typ)

(*********************************************************************************)

let rec replace v subst typ =
  match typ with
  | TInt        -> typ
  | TBool       -> typ
  | TFun (a, b) -> TFun (replace v subst a, replace v subst b)
  | TVar v'     ->
    match v = v' with
    | true  -> subst
    | false -> typ

(*********************************************************************************)

let instantiate fv fresher gtyp =
  let rec inst fv vars typ =
    match vars with
    | Nil            -> Pair (typ, fv)
    | Cons(v, vars') ->
      let new_typ = replace v (TVar fv) typ in
      inst (fresher fv) vars' new_typ in
  match gtyp with
  | Gen (vars, typ) -> inst fv vars typ

(*********************************************************************************)
(*********************************************************************************)

let rec has_var t v =
  match t with
  | TInt        -> false
  | TBool       -> false
  | TVar v'     -> v = v'
  | TFun (a, b) ->
    match has_var a v with
    | true  -> true
    | false -> has_var b v

(*********************************************************************************)

let var_bind v t =
  let has = has_var t v in
  match t with
  | TInt    -> Just (Cons (Pair (v, t), Nil))
  | TBool   -> Just (Cons (Pair (v, t), Nil))
  | TVar v' ->
  begin
    match has with
    | true  -> Just Nil
    | false -> Just (Cons (Pair (v, t), Nil))
  end
  | TFun (a, b) ->
    match has with
    | true  -> Nothing
    | false -> Just (Cons (Pair (v, t), Nil))

(*********************************************************************************)

let rec mgu t1 t2 =
  match t1 with
  | TInt ->
  begin
    match t2 with
    | TInt        -> Just Nil
    | TBool       -> Nothing
    | TVar v      -> var_bind v t1
    | TFun (a, b) -> Nothing
  end
  | TBool ->
  begin
    match t2 with
    | TInt        -> Nothing
    | TBool       -> Just Nil
    | TVar v      -> var_bind v t1
    | TFun (a, b) -> Nothing
  end
  | TVar v      -> var_bind v t2
  | TFun (a, b) ->
    match t2 with
    | TInt          -> Nothing
    | TBool         -> Nothing
    | TVar v        -> var_bind v t1
    | TFun (a', b') ->
      let s1 = mgu a a' in
      let s2 = mb_bind s1 (fun s -> mgu (apply s b) (apply s b')) in
      mb_bind s1 (fun x -> mb_bind s2 (fun y -> Just (compose_subst x y)))

(*********************************************************************************)

let ti_literal l =
  match l with
  | LInt i  -> TInt
  | LBool b -> TBool

(*********************************************************************************)

let rec ti fv fresher env term =
  let ret s t fv = Just (Tuple (s, t, fv)) in

  match term with
  | Var v ->
  begin
    match lookup v env with
    | Nothing -> Nothing
    | Just gt ->
      let pair = instantiate fv fresher gt in
      match pair with
      | Pair (t, fv') -> ret Nil t fv'
  end
  | Lit l       -> ret Nil (ti_literal l) fv
  | Abst (v, b) ->
    let gt    = Gen (Nil, TVar fv) in
    let env'  = Cons (Pair(v, gt), env) in
    let fv'   = fresher fv in
    let tuple = ti fv' fresher env' b in
    mb_bind tuple
    (fun tpl ->
      match tpl with
      | Tuple (s, t, fv'') ->
        ret s (TFun (apply s (TVar fv), t)) fv''
    )
  | App (f, a) ->
    let fv' = fresher fv in
    let tuple1 = ti fv' fresher env f in
    mb_bind tuple1
    (fun tpl1 ->
      match tpl1 with
      | Tuple (s1, t1, fv'') ->
        let tuple2 = ti fv'' fresher (env_apply s1 env) a in
        mb_bind tuple2
        (fun tpl2 ->
          match tpl2 with
          | Tuple (s2, t2, fv''') ->
            let subst = mgu (apply s2 t1) (TFun (t2, TVar fv)) in
            mb_bind subst
            (fun s3 ->
              let s = compose_subst (compose_subst s1 s2) s3 in
              ret s (apply s3 (TVar fv)) fv'''
            )
        )
    )
  | Let (v, a, b) ->
    let tuple1 = ti fv fresher env a in
    mb_bind tuple1
    (fun tpl1 ->
      match tpl1 with
      | Tuple (s1, t1, fv') ->
        let gt1    = generalize (env_apply s1 env) t1 in
        let env'   = Cons (Pair(v, gt1), env) in
        let tuple2 = ti fv' fresher env' b in
        mb_bind tuple2
        (fun tpl2 ->
          match tpl2 with
          | Tuple (s2, t2, fv'') ->
            let s = compose_subst s1 s2 in
            ret s t2 fv''
        )
    )

(*********************************************************************************)

let type_inference first_var fresher term =
 let tuple = ti first_var fresher Nil term in
  mb_bind tuple
  (fun tpl ->
    match tpl with
    | Tuple (s, t, fv) -> Just (apply s t)
  )

(*********************************************************************************)

let nat_type_inference term = type_inference Z (fun n -> S n) term

(*********************************************************************************)

let term1 = Let ("f", Abst("x", Var "x"), App(Var "f", Abst("x", App(Var "f", Var "x"))))
let term2 = App(Abst("f", App(Var "f", Abst("x", App(Var "f", Var "x")))), Abst("x", Var "x"))

*)
