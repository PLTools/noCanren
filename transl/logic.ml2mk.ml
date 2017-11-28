type formula =
  | Val of bool
  | Var of string
  | Conj of formula * formula
  | Disj of formula * formula
  | Not of formula

type subst =
  | Empty
  | Cons of string * bool * subst

type 'a maybe =
  | Nothing
  | Just of 'a

(**************************************************************************)

let bind a f =
  match a with
  | Nothing -> Nothing
  | Just a  -> f a

(**************************************************************************)

let rec lookup s x =
  match s with
  | Empty           -> Nothing
  | Cons (y, v, s') ->
    match x = y with
    | true  -> Just v
    | false -> lookup s' x

(**************************************************************************)

let rec calc_left f s =
  match f with
  | Val v         -> Just v
  | Var x         -> lookup s x
  | Not f'        -> bind (calc_left f' s) (fun x -> Just (x = false))
  | Conj (f1, f2) -> bind (calc_left f1 s) (fun x -> match x with
                                                     | true  -> calc_left f2 s
                                                     | false -> Just false)
  | Disj (f1, f2) -> bind (calc_left f1 s) (fun x -> match x with
                                                     | true -> Just true
                                                     | false -> calc_left f2 s)


(**************************************************************************)

let rec calc_both f s =
  match f with
  | Val v         -> Just v
  | Var x         -> lookup s x
  | Not f'        -> bind (calc_both f' s) (fun x -> Just (x = false))
  | Conj (f1, f2) -> bind (calc_both f1 s) (fun x -> match x with
                                                     | true  -> calc_both f2 s
                                                     | false -> bind (calc_both f2 s) (fun v -> Just false))
  | Disj (f1, f2) -> bind (calc_both f1 s) (fun x -> match x with
                                                     | true -> bind (calc_both f2 s) (fun v -> Just true)
                                                     | false -> calc_both f2 s)


(**************************************************************************)

