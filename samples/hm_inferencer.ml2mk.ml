open List

type num =
  | Z
  | S of num

type ('a, 'b, 'c) tuple = Tuple of 'a * 'b * 'c

type 'a lambda_type =
  | TInt
  | TBool
  | TPair of 'a lambda_type * 'a lambda_type
  | TVar of 'a
  | TFun of 'a lambda_type * 'a lambda_type

type 'a gen_type = Gen of 'a list * 'a lambda_type

type literal =
  | LInt of int
  | LBool of bool

type 'a lambda =
  | Var of 'a
  | Lit of literal
  | Tuple2 of 'a lambda * 'a lambda
  | App of 'a lambda * 'a lambda
  | Abst of 'a * 'a lambda
  | Let of 'a * 'a lambda * 'a lambda

(*********************************************************************************)

let rec remove e l =
  match l with
  | [] -> []
  | e' :: l' -> if e = e' then remove e l' else e' :: remove e l'
;;

let rec remove_list rem l =
  match rem with
  | [] -> l
  | x :: rem' -> remove_list rem' (remove x l)
;;

let rec remove_repeats l =
  match l with
  | [] -> []
  | e' :: l' -> e' :: remove_repeats (remove e' l')
;;

let rec append a b =
  match a with
  | [] -> b
  | x :: a' -> x :: append a' b
;;

let free_vars typ =
  let rec fvars typ =
    match typ with
    | TInt -> []
    | TBool -> []
    | TVar v -> [ v ]
    | TPair (a, b) -> append (fvars a) (fvars b)
    | TFun (a, b) -> append (fvars a) (fvars b)
  in
  remove_repeats (fvars typ)
;;

let gen_free_vars gtyp =
  match gtyp with
  | Gen (vars, t) -> remove_list vars (free_vars t)
;;

let env_free_vars env =
  let rec get_fvs env =
    match env with
    | [] -> []
    | p :: env' ->
      (match p with
       | v, gtyp -> append (gen_free_vars gtyp) (get_fvs env'))
  in
  remove_repeats (get_fvs env)
;;

(*********************************************************************************)

let rec lookup v s =
  match s with
  | p :: xs ->
    (match p with
     | v', t -> if v = v' then t else lookup v xs)
;;

let rec lookup_total v s =
  match s with
  | [] -> None
  | p :: xs ->
    (match p with
     | v', t -> if v = v' then Some t else lookup_total v xs)
;;

(*********************************************************************************)

let rec delete e l =
  match l with
  | [] -> l
  | p :: l' ->
    (match p with
     | e', t -> if e = e' then delete e l' else p :: delete e l')
;;

let rec delete_list del l =
  match del with
  | [] -> l
  | e :: del' -> delete_list del' (delete e l)
;;

let rec map f l =
  match l with
  | [] -> []
  | x :: l' -> f x :: map f l'
;;

(*********************************************************************************)

let mb_bind a f = f a

(*********************************************************************************)
(* Substitute all type variables if their instantiation is already known *)
let rec apply s typ =
  match typ with
  | TInt -> typ
  | TBool -> typ
  | TPair (a, b) -> TPair (apply s a, apply s b)
  | TFun (a, b) -> TFun (apply s a, apply s b)
  | TVar v ->
    (match lookup_total v s with
     | Some typ' -> typ'
     | None -> typ)
;;

(*********************************************************************************)

let gen_apply s gtyp =
  match gtyp with
  | Gen (vars, typ) -> Gen (vars, apply (delete_list vars s) typ)
;;

(*********************************************************************************)

let env_apply s env =
  let member_apply p =
    match p with
    | v, gtyp -> v, gen_apply s gtyp
  in
  map member_apply env
;;

(*********************************************************************************)

let compose_subst s1 s2 =
  let apply_member p =
    match p with
    | k, v -> k, apply s1 v
  in
  append (map apply_member s2) s1
;;

(*********************************************************************************)

let generalize env typ =
  let vars = remove_list (env_free_vars env) (free_vars typ) in
  Gen (vars, typ)
;;

(*********************************************************************************)

let rec replace v subst typ =
  match typ with
  | TInt -> typ
  | TBool -> typ
  | TPair (a, b) -> TPair (replace v subst a, replace v subst b)
  | TFun (a, b) -> TFun (replace v subst a, replace v subst b)
  | TVar v' -> if v = v' then subst else typ
;;

(*********************************************************************************)

let instantiate fv fresher gtyp =
  let rec inst fv vars typ =
    match vars with
    | [] -> typ, fv
    | v :: vars' ->
      let new_typ = replace v (TVar fv) typ in
      inst (fresher fv) vars' new_typ
  in
  match gtyp with
  | Gen (vars, typ) -> inst fv vars typ
;;

(*********************************************************************************)

let rec has_var t v =
  match t with
  | TInt -> false
  | TBool -> false
  | TVar v' -> v = v'
  | TPair (a, b) -> if has_var a v then true else has_var b v
  | TFun (a, b) -> if has_var a v then true else has_var b v
;;

(*********************************************************************************)

let var_bind v t =
  let has = has_var t v in
  match t with
  | TInt -> [ v, t ]
  | TBool -> [ v, t ]
  | TVar v' -> if has then [] else [ v, t ]
  | TPair (a, b) ->
    (match has with
     | false -> [ v, t ])
  | TFun (a, b) ->
    (match has with
     | false -> [ v, t ])
;;

(*********************************************************************************)

let rec mgu t1 t2 =
  match t1 with
  | TInt ->
    (match t2 with
     | TInt -> []
     | TVar v -> var_bind v t1)
  | TBool ->
    (match t2 with
     | TBool -> []
     | TVar v -> var_bind v t1)
  | TVar v -> var_bind v t2
  | TFun (a, b) ->
    (match t2 with
     | TVar v -> var_bind v t1
     | TFun (a', b') ->
       let s1 = mgu a a' in
       let s2 = mb_bind s1 (fun s -> mgu (apply s b) (apply s b')) in
       compose_subst s1 s2)
  | TPair (a, b) ->
    (match t2 with
     | TVar v -> var_bind v t1
     | TPair (a', b') ->
       let s1 = mgu a a' in
       let s2 = mb_bind s1 (fun s -> mgu (apply s b) (apply s b')) in
       mb_bind s1 (fun x -> mb_bind s2 (fun y -> compose_subst x y)))
;;

(*********************************************************************************)

let ti_literal l =
  match l with
  | LInt i -> TInt
  | LBool b -> TBool
;;

(*********************************************************************************)
(* [fv] is the next available name from new type variable
 * [fresher] constructs _next_ available name for new type variable
 * [env] is a mapping from varibles-terms to its types
 * Returns [(env * typ * next_fv) maybe] where
     typ is an answer
     next_fv is next name for type variable
     env is a mapping from type variables to types
 * *)
let rec ti fv fresher env term =
  let ret s t fv = Tuple (s, t, fv) in
  match term with
  | Var v ->
    (match lookup v env with
     | gt ->
       let pair0 = instantiate fv fresher gt in
       (match pair0 with
        | t, fv' -> ret [] t fv'))
  | Lit l -> ret [] (ti_literal l) fv
  | Abst (v, b) ->
    let gt = Gen ([], TVar fv) in
    let env' = (v, gt) :: env in
    let fv' = fresher fv in
    let tuple0 = ti fv' fresher env' b in
    mb_bind tuple0 (fun tpl ->
      match tpl with
      | Tuple (s, t, fv'') -> ret s (TFun (apply s (TVar fv), t)) fv'')
  | Tuple2 (l, r) ->
    let fv' = fresher fv in
    let tuple1 = ti fv' fresher env l in
    mb_bind tuple1 (fun tpl1 ->
      match tpl1 with
      | Tuple (s1, t1, fv'') ->
        let tuple2 = ti fv'' fresher (env_apply s1 env) r in
        mb_bind tuple2 (fun tpl2 ->
          match tpl2 with
          | Tuple (s2, t2, fv''') -> ret (compose_subst s1 s2) (TPair (t1, t2)) fv'''))
  | App (f, a) ->
    let fv' = fresher fv in
    let tuple1 = ti fv' fresher env f in
    mb_bind tuple1 (fun tpl1 ->
      match tpl1 with
      | Tuple (s1, t1, fv'') ->
        let tuple2 = ti fv'' fresher (env_apply s1 env) a in
        mb_bind tuple2 (fun tpl2 ->
          match tpl2 with
          | Tuple (s2, t2, fv''') ->
            let subst = mgu (apply s2 t1) (TFun (t2, TVar fv)) in
            mb_bind subst (fun s3 ->
              let s = compose_subst (compose_subst s1 s2) s3 in
              ret s (apply s3 (TVar fv)) fv''')))
  | Let (v, a, b) ->
    let tuple1 = ti fv fresher env a in
    mb_bind tuple1 (fun tpl1 ->
      match tpl1 with
      | Tuple (s1, t1, fv') ->
        let gt1 = generalize (env_apply s1 env) t1 in
        let env' = (v, gt1) :: env in
        let tuple2 = ti fv' fresher env' b in
        mb_bind tuple2 (fun tpl2 ->
          match tpl2 with
          | Tuple (s2, t2, fv'') ->
            let s = compose_subst s1 s2 in
            ret s t2 fv''))
;;

(*********************************************************************************)

let type_inference first_var fresher term =
  let tuple0 = ti first_var fresher [] term in
  mb_bind tuple0 (fun tpl ->
    match tpl with
    | Tuple (s, t, fv) -> apply s t)
;;

(*********************************************************************************)

let nat_type_inference term = type_inference Z (fun n -> S n) term
