(*
 * MiniKanren: miniKanren implementation.
 * Copyright (C) 2015-2016
 * Dmitri Boulytchev, Dmitry Kosarev, Alexey Syomin,
 * St.Petersburg State University, JetBrains Research
 *
 * This software is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License version 2, as published by the Free Software Foundation.
 *
 * This software is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 *
 * See the GNU Library General Public License version 2 for more details
 * (enclosed in the file COPYING).
 *)

module Stream =
  struct

    type 'a t = Nil | Cons of 'a * 'a t | Lazy of 'a t Lazy.t

    let from_fun (f: unit -> 'a t) : 'a t = Lazy (Lazy.lazy_from_fun f)

    let nil = Nil

    let cons h t = Cons (h, t)

    let rec is_empty = function
    | Nil    -> true
    | Lazy s -> is_empty @@ Lazy.force s
    | _ -> false

    let rec retrieve ?(n=(-1)) s =
      if n = 0
      then [], s
      else match s with
           | Nil          -> [], s
           | Cons (x, xs) -> let xs', s' = retrieve ~n:(n-1) xs in x::xs', s'
           | Lazy  z      -> retrieve ~n:n (Lazy.force z)

    let take ?(n=(-1)) s = fst @@ retrieve ~n:n s

    let hd s = List.hd @@ take ~n:1 s
    let tl s = snd @@ retrieve ~n:1 s

    let rec of_list = function
    | []    -> nil
    | x::xs -> cons x (of_list xs)

    let rec to_list = function
    | Nil           -> []
    | Cons (hd, tl) -> hd :: to_list tl
    | Lazy  z       -> to_list (Lazy.force z)

    let rec mplus fs gs =
      from_fun (fun () ->
         match fs with
         | Nil           -> gs
         | Cons (hd, tl) -> cons hd (mplus gs tl)
         | Lazy z        -> mplus gs (Lazy.force z)
      )

    let rec bind xs f =
      from_fun (fun () ->
        match xs with
        | Cons (x, xs) -> mplus (f x) (bind xs f)
        | Nil          -> nil
        | Lazy z       -> bind (Lazy.force z) f
     )

    let rec map f = function
    | Nil -> Nil
    | Cons (x, xs) -> Cons (f x, map f xs)
    | Lazy s -> Lazy (Lazy.lazy_from_fun (fun () -> map f @@ Lazy.force s))

    let rec iter f = function
    | Nil -> ()
    | Cons (x, xs) -> f x; iter f xs
    | Lazy s -> iter f @@ Lazy.force s

    let rec filter : 'b list -> ('a * 'b) t -> 'a t = fun table -> function
    | Nil -> Nil
    | Cons ((x, h), xs) -> if List.mem h table then filter table xs else Cons (x, filter (h::table) xs)
    | Lazy z -> Lazy (Lazy.lazy_from_fun (fun () -> filter table @@ Lazy.force z))

  end

module SupportStream =
  struct

    type history = Leaf | Left of history | Right of history | Node of history list

    (* let rec print_his = function
    | Leaf -> Printf.printf ""
    | Left h -> Printf.printf "L"; print_his h
    | Right h -> Printf.printf "R"; print_his h
    | Node [] -> Printf.printf "[]"
    | Node (h::hs) -> Printf.printf "["; print_his h; List.map (fun h -> Printf.printf "|"; print_his h) hs; Printf.printf "]" *)

    type 'a t = Nil | Diverge of bool | Cons of ('a * history) * 'a t | Lazy of 'a t Lazy.t

    let from_fun (f: unit -> 'a t) : 'a t = Lazy (Lazy.lazy_from_fun f)

    let nil = Nil

    let divergence = Diverge false

    let cons h t = Cons ((h, Leaf), t)

    let rec his_map hf ss =
      from_fun (fun () ->
        match ss with
        | Nil                -> Nil
        | Diverge b          -> Diverge b
        | Cons ((x, h), xss) -> Cons ((x, hf h), his_map hf xss)
        | Lazy z             -> his_map hf (Lazy.force z)
      )

    let start_node ss = his_map (fun h -> Node [h]) ss

    let rec mplus fss gss =
      from_fun (fun () ->
        match fss with
        | Nil -> gss
        | Diverge b -> Diverge b
        | Cons (p, xss) -> Cons (p, mplus xss gss)
        | Lazy z -> mplus gss (Lazy.force z)
      )

    let mplus_s fss gss =
      let rec mplus_s side fss gss =
        from_fun (fun () ->
          match fss with
          | Nil                -> his_map (if side then (fun h -> Left h) else (fun h -> Right h)) gss
          | Diverge b          -> Diverge b
          | Cons ((x, h), xss) -> Cons ((x, if side then Right h else Left h), (mplus_s side xss gss))
          | Lazy z             -> mplus_s (not side) gss (Lazy.force z)
        )
      in mplus_s false fss gss

    let rec bind ss f =
      from_fun (fun () ->
        match ss with
        | Nil                -> Nil
        | Diverge _          -> Diverge true
        | Cons ((x, h), xss) -> mplus (his_map (fun (Node hs) -> Node (h::hs)) (f x)) (bind xss f)
        | Lazy z             -> bind (Lazy.force z) f
      )

    let rec catch ss hs =
      from_fun (fun () ->
        match ss with
        | Nil           -> Nil
        | Diverge false -> Diverge false
        | Diverge true  -> hs
        | Cons (p, xss) -> Cons (p, catch xss hs)
        | Lazy z        -> catch (Lazy.force z) hs
      )

    let shift i =
      let rec insert : 'a -> int -> 'a list -> 'a list = fun e i xs ->
        match i, xs with
        | 0, _      -> e :: xs
        | _, []     -> [e]
        | _, hd::tl -> hd :: insert e (i - 1) tl
      in function
      | Node (h :: hs) -> Node (insert h i hs)
      | Leaf -> Leaf
      | Left h -> Left h
      | Right h -> Right h
      | Node [] -> Node []

    let shift_first i ss = his_map (shift i) ss

    let rec to_h_stream ss hs =
      match ss with
      | Nil           -> Stream.nil
      | Diverge _     -> hs
      | Cons (p, xss) -> Stream.cons p (to_h_stream xss hs)
      | Lazy z        -> Stream.from_fun (fun () -> to_h_stream (Lazy.force z) hs)

  end

let (!!!) = Obj.magic;;

@type 'a logic = Ident of GT.int GT.list * GT.int | Var of GT.int GT.list * GT.int * 'a logic GT.list | Value of 'a
with show, gmap

let logic = {logic with
  gcata = ();
  plugins =
    object
      method gmap    = logic.plugins#gmap
      method show fa x =
        GT.transform(logic)
           (GT.lift fa)
           (object inherit ['a] @logic[show]
              method c_Var _ s _ i cs =
                let c =
                  match cs with
                  | [] -> ""
                  | _  -> Printf.sprintf " %s" (GT.show(GT.list) (fun l -> "=/= " ^ s.GT.f () l) cs)
                in
                Printf.sprintf "_.%d%s" i c

              method c_Value _ _ x = x.GT.fx ()
            end)
           ()
           x
    end
};;

@type 'a unlogic = [`Var of GT.int * 'a logic GT.list | `Value of 'a]
with show, gmap

let destruct = function
| Var (_, i, c) -> `Var (i, c)
| Value x       -> `Value x

exception Not_a_value

let (!!) x = Value x
let inj = (!!)

let prj_k k = function Value x -> x | Var (_, i, c) -> k i c
let prj x = prj_k (fun _ -> raise Not_a_value) x

let (!?) = prj

exception Occurs_check

type w = Unboxed of Obj.t | Boxed of int * int * (int -> Obj.t) | Invalid of int

let rec wrap (x : Obj.t) =
  Obj.(
    let is_valid_tag =
      List.fold_left
      (fun f t tag -> tag <> t && f tag)
      (fun _ -> true)
      [lazy_tag   ; closure_tag  ; object_tag  ; infix_tag ;
       forward_tag; no_scan_tag  ; abstract_tag; custom_tag;
       final_tag  ; unaligned_tag; out_of_heap_tag
      ]
    in
    let is_unboxed obj =
      is_int obj ||
      (fun t -> t = string_tag || t = double_tag) (tag obj)
    in
    if is_unboxed x
    then Unboxed x
    else
      let t = tag x in
      if is_valid_tag t
      then
        let f = if t = double_array_tag then !!! double_field else field in
        Boxed (t, size x, f x)
      else Invalid t
    )

let generic_show x =
  let x = Obj.repr x in
  let b = Buffer.create 1024 in
  let rec inner o =
    match wrap o with
    | Invalid n             -> Buffer.add_string b (Printf.sprintf "<invalid %d>" n)
    | Unboxed n when !!!n=0 -> Buffer.add_string b "[]"
    | Unboxed n             -> Buffer.add_string b (Printf.sprintf "int<%d>" (!!!n))
    | Boxed (t,l,f) when t=0 && l=1 && (match wrap (f 0) with Unboxed i when !!!i >=10 -> true | _ -> false) ->
       Printf.bprintf b "var%d" (match wrap (f 0) with Unboxed i -> !!!i | _ -> failwith "shit")

    | Boxed   (t, l, f) ->
        Buffer.add_string b (Printf.sprintf "boxed %d <" t);
        for i = 0 to l - 1 do (inner (f i); if i<l-1 then Buffer.add_string b " ") done;
        Buffer.add_string b ">"
  in
  inner x;
  Buffer.contents b

module Env :
  sig
    type t

    val empty  : unit -> t
    val fresh  : t -> 'a logic * t
    val var    : t -> 'a logic -> int option
  end =
  struct
    type t = GT.int GT.list * int

    let empty () = ([0], 10)

    let fresh (a, current) =
      let v = Var (a, current, []) in
      (!!!v, (a, current+1))

    let var_tag, var_size =
      let v = Var ([], 0, []) in
      Obj.tag (!!! v), Obj.size (!!! v)

    let var (a, _) x =
      let t = !!! x in
      if Obj.tag  t = var_tag  &&
         Obj.size t = var_size &&
         (let q = Obj.field t 0 in
          not (Obj.is_int q) && q == (!!!a)
         )
      then let Var (_, i, _) = !!! x in Some i
      else None

  end

type term = Obj.t

let term_of_logic = (!!!)
let logic_of_term = (!!!)

let (^~) hd tl =  term_of_logic hd :: tl
let (^.) a  b  = [term_of_logic a; term_of_logic b]
let (!^) a     = [term_of_logic a]

module IdentSet =
  struct
    module M = Map.Make (struct type t = int let compare = Pervasives.compare end)

    type t = GT.int GT.list * term M.t

    let empty () = ([0], M.empty)

    let new_ident (a, m) id = Ident (a, id)

    let id : t -> term -> int option = fun (a, _) t ->
      let tag, size =
        let v = Ident ([], 0)
        in Obj.tag (!!! v), Obj.size (!!! v)
      in
      if Obj.tag  t = tag  &&
         Obj.size t = size &&
         (let q = Obj.field t 0 in
          not (Obj.is_int q) && q == (!!!a)
         )
      then let Ident (_, i) = !!! t in Some i
      else None

    let bind_ident : term -> term -> t -> t = fun x t ((a, m) as iset) ->
      let Some i = id iset x
      in (a, M.add i t m)

    let rec refine : Env.t -> t -> term -> term = fun env ((a, m) as iset) term ->
      match id iset term with
      | Some i -> M.find i m
      | None ->
        (match Env.var env (!!! term) with
         | Some _ -> term
         | None   ->
            (match wrap (Obj.repr term) with
             | Unboxed _       -> term
             | Boxed (t, s, f) ->
                let term = Obj.dup (Obj.repr term) in
                let sf =
                  if t = Obj.double_array_tag
                  then !!! Obj.set_double_field
                  else Obj.set_field
                in
                for i = 0 to s - 1 do
                  sf term i (refine env iset (!!! (f i)))
                done;
                term
             | Invalid _       -> invalid_arg ""
            )
        )

    let rec idents : t -> term -> int list = fun iset term ->
      match id iset term with
      | Some i -> [i]
      | None ->
        (match wrap (Obj.repr term) with
         | Unboxed _       -> []
         | Boxed (t, s, f) ->
            let rec inner i acc =
              if i < s
              then inner (i+1) (idents iset (!!! (f i)) @ acc)
              else acc
            in inner 0 []
         | Invalid _       -> invalid_arg ""
        )

  end

module Subst :
  sig
    type t

    val empty   : t

    val of_list : (int * Obj.t * Obj.t) list -> t
    val split   : t -> Obj.t list * Obj.t list
    val walk    : Env.t -> 'a logic -> t -> 'a logic
    val unify   : Env.t -> 'a logic -> 'a logic -> t option -> (int * Obj.t * Obj.t) list * t option
    val show    : t -> string
  end =
  struct
    module M = Map.Make (struct type t = int let compare = Pervasives.compare end)

    type t = (Obj.t * Obj.t) M.t

    let show m = (M.fold (fun i (_, x) s -> s ^ Printf.sprintf "%d -> %s; " i (generic_show x)) m "subst {") ^ "}"

    let empty = M.empty

    let of_list l = List.fold_left (fun s (i, v, t) -> M.add i (v, t) s) empty l

    let split s = M.fold (fun _ (x, t) (xs, ts) -> x::xs, t::ts) s ([], [])

    let rec walk env var subst =
      match Env.var env var with
      | None   -> var
      | Some i ->
          try walk env (snd (M.find i (!!! subst))) subst with Not_found -> var

    let rec occurs env xi term subst =
      let y = walk env term subst in
      match Env.var env y with
      | Some yi -> xi = yi
      | None ->
         let wy = wrap (Obj.repr y) in
         match wy with
         | Unboxed _ -> false
         | Invalid n -> invalid_arg (Printf.sprintf "Invalid value in occurs check (%d)" n)
         | Boxed (_, s, f) ->
            let rec inner i =
              if i >= s then false
              else occurs env xi (!!!(f i)) subst || inner (i+1)
            in
            inner 0

    let unify env x y subst =
      let rec unify x y (delta, subst) =
        let extend xi x term delta subst =
          if occurs env xi term subst then raise Occurs_check
          else (xi, !!!x, !!!term)::delta, Some (!!! (M.add xi (!!!x, term) (!!! subst)))
        in
        match subst with
        | None -> delta, None
        | (Some subst) as s ->
            let x, y = walk env x subst, walk env y subst in
            match Env.var env x, Env.var env y with
            | Some xi, Some yi -> if xi = yi then delta, s else extend xi x y delta subst
            | Some xi, _       -> extend xi x y delta subst
            | _      , Some yi -> extend yi y x delta subst
            | _ ->
                let wx, wy = wrap (Obj.repr x), wrap (Obj.repr y) in
                (match wx, wy with
                 | Unboxed vx, Unboxed vy -> if vx = vy then delta, s else delta, None
                 | Boxed (tx, sx, fx), Boxed (ty, sy, fy) ->
                    if tx = ty && sx = sy
                    then
                      let rec inner i (delta, subst) =
                        match subst with
                        | None -> delta, None
                        | Some _ ->
                           if i < sx
                           then inner (i+1) (unify (!!!(fx i)) (!!!(fy i)) (delta, subst))
                           else delta, subst
                      in
                      inner 0 (delta, s)
                    else delta, None
                 | Invalid n, _
                 | _, Invalid n -> invalid_arg (Printf.sprintf "Invalid values for unification (%d)" n)
                 | _ -> delta, None
                )
      in
      unify x y ([], subst)

  end

module State =
  struct
    type t = Env.t * Subst.t * Subst.t list
    let empty () = (Env.empty (), Subst.empty, [])
    let env   (env, _, _) = env
    let show  (env, subst, constr) = Printf.sprintf "st {%s, %s}" (Subst.show subst) (GT.show(GT.list) Subst.show constr)
  end


exception Disequality_violated

let run_unification x y (env, subst, constr) =
  try
    let prefix, subst' = Subst.unify env x y (Some subst) in
    begin match subst' with
    | None -> Stream.nil
    | Some s ->
        try
          (* TODO: only apply constraints with the relevant vars *)
          let constr' =
            List.fold_left (fun css' cs ->
              let x, t  = Subst.split cs in
              try
                let p, s' = Subst.unify env (!!!x) (!!!t) subst' in
                match s' with
                | None -> css'
                | Some _ ->
                    match p with
                    | [] -> raise Disequality_violated
                    | _  -> (Subst.of_list p)::css'
              with Occurs_check -> css'
            )
            []
            constr
          in
          Stream.cons (env, s, constr') Stream.nil
        with Disequality_violated -> Stream.nil
    end
  with Occurs_check -> Stream.nil

let run_unification_sup x y (env, subst, constr) =
  try
    let prefix, subst' = Subst.unify env x y (Some subst) in
    begin match subst' with
    | None -> SupportStream.nil
    | Some s ->
        try
          (* TODO: only apply constraints with the relevant vars *)
          let constr' =
            List.fold_left (fun css' cs ->
              let x, t  = Subst.split cs in
              try
                let p, s' = Subst.unify env (!!!x) (!!!t) subst' in
                match s' with
                | None -> css'
                | Some _ ->
                    match p with
                    | [] -> raise Disequality_violated
                    | _  -> (Subst.of_list p)::css'
              with Occurs_check -> css'
            )
            []
            constr
          in
          SupportStream.cons (env, s, constr') SupportStream.nil
        with Disequality_violated -> SupportStream.nil
    end
  with Occurs_check -> SupportStream.nil

let run_disequality x y ((env, subst, constr) as st) =
  let normalize_store prefix constr =
    let subst  = Subst.of_list prefix in
    let prefix = List.split (List.map (fun (_, x, t) -> (x, t)) prefix) in
    let subsumes subst (vs, ts) =
      try
        match Subst.unify env !!!vs !!!ts (Some subst) with
        | [], Some _ -> true
        | _ -> false
      with Occurs_check -> false
    in
    let rec traverse = function
    | [] -> [subst]
    | (c::cs) as ccs ->
        if subsumes subst (Subst.split c)
        then ccs
        else if subsumes c prefix
             then traverse cs
             else c :: traverse cs
    in
    traverse constr
  in
  try
    let prefix, subst' = Subst.unify env x y (Some subst) in
    match subst' with
    | None -> Stream.cons st Stream.nil
    | Some s ->
        (match prefix with
        | [] -> Stream.nil
        | _  -> Stream.cons (env, subst, normalize_store prefix constr) Stream.nil
        )
  with Occurs_check -> Stream.cons st Stream.nil

let run_disequality_sup x y ((env, subst, constr) as st) =
  let normalize_store prefix constr =
    let subst  = Subst.of_list prefix in
    let prefix = List.split (List.map (fun (_, x, t) -> (x, t)) prefix) in
    let subsumes subst (vs, ts) =
      try
        match Subst.unify env !!!vs !!!ts (Some subst) with
        | [], Some _ -> true
        | _ -> false
      with Occurs_check -> false
    in
    let rec traverse = function
    | [] -> [subst]
    | (c::cs) as ccs ->
        if subsumes subst (Subst.split c)
        then ccs
        else if subsumes c prefix
             then traverse cs
             else c :: traverse cs
    in
    traverse constr
  in
  try
    let prefix, subst' = Subst.unify env x y (Some subst) in
    match subst' with
    | None -> SupportStream.cons st SupportStream.nil
    | Some s ->
        (match prefix with
        | [] -> SupportStream.nil
        | _  -> SupportStream.cons (env, subst, normalize_store prefix constr) SupportStream.nil
        )
  with Occurs_check -> SupportStream.cons st SupportStream.nil

type goal =
| Unification of term * term
| Disequality of term * term
| Conjunction of (goal * int) list
| Disjunction of goal list
| Fresh       of term * goal
| Invoke      of string * term list
| Marked      of goal

let (===) x y = Unification (term_of_logic x, term_of_logic y)

let (=/=) x y = Disequality (term_of_logic x, term_of_logic y)

let conj g1 g2 = Conjunction [(g1, 0); (g2, 0)]

let (&&&) = conj

let disj g1 g2 = Disjunction [g1; g2]

let (|||) = disj

let rec (?|) gs = Disjunction gs

let rec (?&) gs = Conjunction (List.map (fun g -> (g, 0)) gs)

let conde = (?|)

let fresh is gs = List.fold_left (fun g i -> Fresh (i, g)) (?& gs) is

let invoke name args = Invoke (name, args)

type definition = string * (term list * goal)

let def name args body = name, (args, body)

type program = IdentSet.t * definition list * goal

let prog iset defs g = (iset, defs, g)

let call_fresh f (env, subst, constr) =
  let x, env' = Env.fresh env in
  f x (env', subst, constr)

(*
let stream_conj_two f g st = Stream.bind (f st) g

let rec stream_conj = function
| [h]  -> h
| h::t -> stream_conj_two h (stream_conj t)

let stream_disj_two f g st = Stream.mplus (f st) (g st)

let rec stream_disj = function
| [h]  -> h
| h::t -> stream_disj_two h (stream_disj t)
*)

(* module Fresh =
  struct

    let succ prev f = call_fresh (fun x -> prev (f x))

    let zero  f = f
    let one   f = succ zero f
    let two   f = succ one f
    let three f = succ two f
    let four  f = succ three f
    let five  f = succ four f

    let q     = one
    let qr    = two
    let qrs   = three
    let qrst  = four
    let pqrst = five

  end *)

let success = (!!true === !!true)
let failure = (!!true === !!false)

let eqo x y t =
  conde [
    (x === y) &&& (t === !!true);
    (x =/= y) &&& (t === !!false);
  ]

let neqo x y t =
  conde [
    (x =/= y) &&& (t === !!true);
    (x === y) &&& (t === !!false);
  ];;

@type ('a, 'l) llist = Nil | Cons of 'a * 'l with show, gmap
@type 'a lnat = O | S of 'a with show, gmap

module Bool =
  struct

    type 'a logic' = 'a logic
    let logic' = logic

    type ground = bool

    let ground = {
      GT.gcata = ();
      GT.plugins =
        object(this)
          method gmap    n   = GT.gmap   (GT.bool) n
          method show    n   = GT.show   (GT.bool) n
        end
    }

    type logic = bool logic'

    let logic = {
      GT.gcata = ();
      GT.plugins =
        object(this)
          method gmap    n   = GT.gmap   (logic') (GT.gmap   (ground)) n
          method show    n   = GT.show   (logic') (GT.show   (ground)) n
        end
    }

    let (!) = (!!)

    (* let (|^) a b c =
      conde [
        (a === !false) &&& (b === !false) &&& (c === !true);
        (a === !false) &&& (b === !true)  &&& (c === !true);
        (a === !true)  &&& (b === !false) &&& (c === !true);
        (a === !true)  &&& (b === !true)  &&& (c === !false);
      ]

    let noto' a na = (a |^ a) na

    let noto a = noto' a !true

    let oro a b c =
      Fresh.two (fun aa bb ->
        ((a  |^ a) aa) &&&
        ((b  |^ b) bb) &&&
        ((aa |^ bb) c)
      )

    let ando a b c =
      Fresh.one (fun ab ->
        ((a  |^ b) ab) &&&
        ((ab |^ ab) c)
      )

    let (&&) a b = ando a b !true
    let (||) a b = oro  a b !true *)

  end

module Nat =
  struct

    type 'a logic' = 'a logic
    let logic' = logic

    type 'a t = 'a lnat

    type ground = ground t

    let ground = {
      GT.gcata = ();
      GT.plugins =
        object(this)
          method gmap    n = GT.gmap   (lnat) this#gmap    n
          method show    n = GT.show   (lnat) this#show    n
        end
    }

    type logic  = logic t logic'

    let logic = {
      GT.gcata = ();
      GT.plugins =
        object(this)
          method gmap    n   = GT.gmap   (logic') (GT.gmap   (lnat) this#gmap   ) n
          method show    n   = GT.show   (logic') (GT.show   (lnat) this#show   ) n
        end
    }

    let rec of_int n = if n <= 0 then O else S (of_int (n-1))
    let rec to_int   = function O -> 0 | S n -> 1 + to_int n

    let (!) = (!!)

    let rec inj n = ! (GT.gmap(lnat) inj n)

    let prj_k k n =
      let rec inner n =
        GT.gmap(lnat) inner (prj_k k n)
      in
      inner n

    let prj n = prj_k (fun _ -> raise Not_a_value) n

    (* let rec addo x y z =
      conde [
        (x === !O) &&& (z === y);
        Fresh.two (fun x' z' ->
           (x === !(S x')) &&&
           (z === !(S z')) &&&
           (addo x' y z')
        )
      ]

    let (+) = addo

    let rec mulo x y z =
      conde [
        (x === !O) &&& (z === !O);
        Fresh.two (fun x' z' ->
          (x === !(S x')) &&&
          (addo y z' z) &&&
          (mulo x' y z')
        )
      ]

    let ( * ) = mulo

    let rec leo x y b =
      conde [
        (x === !O) &&& (b === !true);
        Fresh.two (fun x' y' ->
          conde [
            (x === !(S x')) &&& (y === !(S y')) &&& (leo x' y' b)
          ]
        )
      ]

    let geo x y b = leo y x b

    let (<=) x y = leo x y !true
    let (>=) x y = geo x y !true

    let gto x y b = conde [(x >= y) &&& (x =/= y) &&& (b === !true)]
    let lto x y b = gto y x b

    let (>) x y = gto x y !true
    let (<) x y = lto x y !true *)

  end

let rec inj_nat n =
  if n <= 0 then inj O
  else inj (S (inj_nat @@ n-1))

let rec prj_nat n =
  match prj n with
  | O   -> 0
  | S n -> 1 + prj_nat n

module List =
  struct

    include List

    type 'a logic' = 'a logic

    let logic' = logic

    type ('a, 'l) t = ('a, 'l) llist

    type 'a ground = ('a, 'a ground) t
    type 'a logic  = ('a, 'a logic)  t logic'

    let rec of_list = function [] -> Nil | x::xs -> Cons (x, of_list xs)
    let rec to_list = function Nil -> [] | Cons (x, xs) -> x::to_list xs

    let (%)  x y = !!(Cons (x, y))
    let (%<) x y = !!(Cons (x, !!(Cons (y, !!Nil))))
    let (!<) x   = !!(Cons (x, !!Nil))

    let nil = inj Nil

    let rec inj fa l = !! (GT.gmap(llist) fa (inj fa) l)

    let prj_k fa k l =
      let rec inner l =
        GT.gmap(llist) fa inner (prj_k k l)
      in
      inner l

    let prj fa l = prj_k fa (fun _ -> raise Not_a_value) l

    let ground = {
      GT.gcata = ();
      GT.plugins =
        object(this)
          method gmap    fa l = GT.gmap   (llist) fa (this#gmap    fa) l
          method show    fa l = "[" ^
            let rec inner l =
              (GT.transform(llist)
                 (GT.lift fa)
                 (GT.lift inner)
                 (object inherit ['a,'a ground] @llist[show]
                    method c_Nil   _ _      = ""
                    method c_Cons  i s x xs = x.GT.fx () ^ (match xs.GT.x with Nil -> "" | _ -> "; " ^ xs.GT.fx ())
                  end)
                 ()
                 l
              )
            in inner l ^ "]"
        end
    }

    let logic = {
      GT.gcata = ();
      GT.plugins =
        object(this)
          method gmap    fa l   = GT.gmap   (logic') (GT.gmap   (llist) fa (this#gmap    fa)) l
          method show    fa l =
            GT.show(logic')
              (fun l -> "[" ^
                 let rec inner l =
                   (GT.transform(llist)
                      (GT.lift fa)
                      (GT.lift (GT.show(logic) inner))
                      (object inherit ['a,'a logic] @llist[show]
                         method c_Nil   _ _      = ""
                         method c_Cons  i s x xs = x.GT.fx () ^ (match xs.GT.x with Value Nil -> "" | _ -> "; " ^ xs.GT.fx ())
                       end)
                      ()
                      l
                   )
                 in inner l ^ "]"
              )
              l
        end
    }

    let (!) = (!!)

    (* let rec foldro f a xs r =
      conde [
        (xs === !Nil) &&& (a === r);
        Fresh.three (
          fun h t a'->
            (xs === h % t) &&&
            (f h a' r) &&&
            (foldro f a t a')
        )
      ]

    let rec mapo f xs ys =
      conde [
        (xs === !Nil) &&& (ys === !Nil);
        Fresh.two (
          fun z zs ->
            (xs === z % zs) &&&
            (Fresh.two (
               fun a1 a2 ->
                  (ys === a1 % a2) &&&
                 (f z a1) &&&
                 (mapo f zs a2)
            ))
        )
      ]

    let filtero p xs ys =
      let folder x a a' =
        conde [
          (p x !true) &&& (x % a === a');
          (p x !false) &&& (a === a')
        ]
      in
      foldro folder !Nil xs ys

    let rec lookupo p xs mx =
      conde [
        (xs === !Nil) &&& (mx === !None);
        Fresh.two (
          fun h t ->
             (h % t === xs) &&&
             (conde [
                (p h !true) &&& (mx === !(Some h));
                (p h !false) &&& (lookupo p t mx)
             ])
        )
      ]

    let anyo = foldro Bool.oro !false

    let allo = foldro Bool.ando !true

    let rec lengtho l n =
      conde [
        (l === !Nil) &&& (n === !O);
        Fresh.three (fun x xs n' ->
          (l === x % xs)  &&&
          (n === !(S n')) &&&
          (lengtho xs n')
        )
      ]

    let rec appendo a b ab =
      conde [
        (a === !Nil) &&& (b === ab);
        Fresh.three (fun h t ab' ->
          (a === h%t) &&&
          (h%ab' === ab) &&&
          (appendo t b ab')
        )
      ]

    let rec reverso a b =
      conde [
        (a === !Nil) &&& (b === !Nil);
        Fresh.three (fun h t a' ->
          (a === h%t) &&&
          (appendo a' !<h b) &&&
          (reverso t a')
        )
      ]

    let rec membero l a =
      Fresh.two (fun x xs ->
        (l === x % xs) &&&
        (conde [
           x === a;
           (x =/= a) &&& (membero xs a)
         ])
      ) *)
  end

let rec inj_list = function
| []    -> inj Nil
| x::xs -> inj (Cons (inj x, inj_list xs))

let rec prj_list l =
  match prj l with
  | Nil -> []
  | Cons (x, xs) -> prj x :: prj_list xs

let (%)  = List.(%)
let (%<) = List.(%<)
let (!<) = List.(!<)
let nil  = List.nil

let rec inj_nat_list = function
| []    -> !!Nil
| x::xs -> inj_nat x % inj_nat_list xs

let rec prj_nat_list l =
  match prj l with
  | Nil -> []
  | Cons (x, xs) -> prj_nat x :: prj_nat_list xs


let rec refine : State.t -> 'a logic -> 'a logic = fun ((e, s, c) as st) x ->
  let rec walk' recursive env var subst =
    let var = Subst.walk env var subst in
    match Env.var env var with
    | None ->
        (match wrap (Obj.repr var) with
         | Unboxed _ -> !!!var
         | Boxed (t, s, f) ->
            let var = Obj.dup (Obj.repr var) in
            let sf =
              if t = Obj.double_array_tag
              then !!! Obj.set_double_field
              else Obj.set_field
            in
            for i = 0 to s - 1 do
              sf var i (!!!(walk' true env (!!!(f i)) subst))
           done;
           !!!var
         | Invalid n -> invalid_arg (Printf.sprintf "Invalid value for reconstruction (%d)" n)
        )
    | Some i when recursive ->
        (match var with
         | Var (a, i, _) ->
            let cs =
              List.fold_left
                (fun acc s ->
                   match walk' false env (!!!var) s with
                   | Var (_, j, _) when i = j -> acc
                   | t -> (refine st t) :: acc
                )
                []
                c
            in
            Var (a, i, cs)
        )
    | _ -> var
  in
  walk' true e (!!!x) s



(*** Interpretation ***)

module Interpretation :
  sig

    val run_prog : program -> State.t -> State.t Stream.t

    val final_run : program -> State.t -> State.t Stream.t

  end =
  struct

    module IntMap = Map.Make (struct type t = int let compare = Pervasives.compare end)

    let rec is_reducible : Env.t -> 'a logic -> 'a logic -> bool = fun env term res_term ->
      let rec equal : 'a logic -> 'a logic -> bool = fun t1 t2 ->
        match Env.var env t1, Env.var env t2 with
        | Some i, Some j -> i = j
        | None  , Some _ -> false
        | Some _, None   -> false
        | None  , None   ->
          (
            match wrap (Obj.repr t1), wrap (Obj.repr t2) with
            | Unboxed _      , Unboxed _          -> t1 = t2
            | Boxed (t, s, f), Boxed (t1, s1, f1) ->
              if t = t1 && s = s1
              then
                let rec inner i =
                  if i < s
                  then equal (!!!(f i)) (!!!(f1 i)) && inner (i + 1)
                  else true
                in
                inner 0
              else false
            | _              , _                  -> false
          )
      in
      let rec find_subst : term IntMap.t option -> 'a logic -> 'a logic -> term IntMap.t option = fun subst_opt term res_term ->
        match subst_opt with
        | None -> None
        | Some subst ->
          (
            match Env.var env term with
            | Some i ->
              (
                let v_opt = try Some (IntMap.find i subst) with Not_found -> None in
                match v_opt with
                | Some v -> if equal (!!! v) (!!! res_term) then subst_opt else None
                | None   -> Some (IntMap.add i (term_of_logic res_term) subst)
              )
            | None   ->
              (
                match Env.var env res_term with
                | Some _ -> None
                | None   ->
                  (
                    match wrap (Obj.repr term), wrap (Obj.repr res_term) with
                    | Unboxed _      , Unboxed _          -> if term = res_term then subst_opt else None
                    | Boxed (t, s, f), Boxed (rt, rs, rf) ->
                      if t = rt && s = rs
                      then
                        let rec drag_subst i subs =
                          if i < s
                          then drag_subst (i + 1) (find_subst subs (!!!(f i)) (!!!(rf i)))
                          else subs
                        in
                        drag_subst 0 subst_opt
                      else None
                    | _              , _                  ->  None
                  )
              )
          )
      in
      match find_subst (Some IntMap.empty) term res_term with
      | Some _ -> true
      | None   -> false

    module StringMap = Map.Make (struct type t = string let compare = Pervasives.compare end)

    module Log = StringMap

    module Defs = StringMap

    exception Divergence

    type result =
    | Result of State.t list
    | Diverge
    | RebuildFunc of goal
    (* | Restart of (term list * goal) Defs.t *)
    | Restart of string * (term list * goal) Defs.t

    let process_result : (State.t list -> result) -> (goal -> result) -> result -> result =
      fun proc_result proc_rebuild ->
        function
        | Result      s    -> proc_result s
        | Diverge          -> Diverge
        | RebuildFunc g    -> proc_rebuild g
        (* | Restart     defs -> Restart defs *)
        | Restart     (name, defs) -> Restart (name, defs)

    let rec pre_opt goal =
      let rec expand_conj : (goal * int) list -> (goal * int) list = function
      | []                        -> []
      | (Conjunction cs, i) :: tl -> cs @ expand_conj tl
      | hd :: tl                  -> hd :: expand_conj tl
      in
      match goal with
      | Unification _     -> goal
      | Disequality _     -> goal
      | Invoke      _     -> goal
      | Disjunction gs    -> Disjunction (List.map pre_opt gs)
      | Fresh      (x, g) -> Fresh (x, pre_opt g)
      | Marked      g     -> Marked (pre_opt g)
      | Conjunction cs    -> Conjunction (expand_conj @@ List.map (fun (g, i) -> (pre_opt g, i)) cs)

    let run_prog (iset, defs, goal) =
      let defs, goal = List.map (fun (name, (args, body)) -> (name, (args, pre_opt body))) defs, pre_opt goal in
      let defs = List.fold_left (fun acc (k, x) -> Defs.add k x acc) Defs.empty defs in
      let rec swap_first_unmarked : goal * int -> (goal * int) list -> ((goal * int) * (goal * int) list) option = fun sg ->
        function
        | []                  -> None
        | (Marked g, i) :: gs -> (match swap_first_unmarked sg gs with None -> None | Some (rg, gs') -> Some (rg, (Marked g, i) :: gs'))
        | (g, i)             :: gs -> let (a, b) = sg in Some ((g, i), (Marked a, b) :: gs)
      in
      (* let rec run_goal : (term list * goal) Defs.t -> term Log.t -> IdentSet.t -> goal -> State.t -> result = fun defs log iset goal ((env, subst, constr) as st) -> *)
      let rec run_goal : (term list * goal) Defs.t -> (term * int) Log.t -> IdentSet.t -> goal -> State.t -> result = fun defs log iset goal ((env, subst, constr) as st) ->

        match goal with
        | Unification (x, y)           -> Result (Stream.to_list (run_unification (logic_of_term @@ IdentSet.refine env iset x)
                                                                                  (logic_of_term @@ IdentSet.refine env iset y)
                                                                                  st))
        | Disequality (x, y)           -> Result (Stream.to_list (run_disequality (logic_of_term @@ IdentSet.refine env iset x)
                                                                                  (logic_of_term @@ IdentSet.refine env iset y)
                                                                                  st))
        | Marked       g               -> process_result (fun ss -> Result ss) (fun g' -> RebuildFunc (Marked g')) (run_goal defs log iset g st)
        | Fresh       (x, g)           -> let var, env' = Env.fresh env in
                                          process_result (fun ss -> Result ss)
                                                         (fun g' -> RebuildFunc (Fresh (x, g')))
                                                         (run_goal defs log (IdentSet.bind_ident x (term_of_logic var) iset) g (env', subst, constr))
        | Disjunction [g]              -> process_result (fun ss -> Result ss) (fun g' -> RebuildFunc (Disjunction [g'])) (run_goal defs log iset g st)
        | Disjunction (g :: gs)        -> process_result (fun g_res -> process_result (fun gs_res -> Result (g_res @ gs_res))
                                                                                      (function (Disjunction gs') -> RebuildFunc (Disjunction (g :: gs')))
                                                                                      (run_goal defs log iset (Disjunction gs) st))
                                                         (fun g' -> RebuildFunc (Disjunction (g' :: gs)))
                                                         (run_goal defs log iset g st)
        | Conjunction [(g, i)]              -> process_result (fun ss -> Result ss) (fun g' -> RebuildFunc (Conjunction [(g', i)])) (run_goal defs log iset g st)
        | Conjunction ((Marked g, i) :: gs) -> run_goal defs log iset (Conjunction ((g, 0) :: List.map (function (Marked mg, i) -> (mg, i) | (g, i) -> (g, i)) gs)) st
        | Conjunction ((g, i) :: gs)        -> (
                                             match run_goal defs log iset g st with
                                             | Result ss      ->
                                                 List.fold_right (fun s -> function
                                                                           | Result acc -> process_result (fun s_res -> Result (s_res @ acc))
                                                                                                          (function (Conjunction gs') -> RebuildFunc (Conjunction ((g, i) :: gs')))
                                                                                                          (run_goal defs log iset (Conjunction gs) s)
                                                                           | res -> res
                                                                 ) ss (Result [])
                                             | Diverge        -> (
                                                                    match swap_first_unmarked (g, i) gs with
                                                                    | None -> Diverge
                                                                    | Some (g', gs') -> RebuildFunc (Conjunction (g' :: gs'))
                                                                 )
                                             | RebuildFunc g' -> RebuildFunc (Conjunction ((g', i) :: gs))
                                             (* | Restart defs'  -> Restart defs' *)
                                             | Restart (name, defs')  -> Restart (name, defs')
                                          )
        | Invoke (name, arg_vals)      ->
                                          let arg_vals_refined = List.map (fun av -> term_of_logic (refine st (logic_of_term (IdentSet.refine env iset av)))) arg_vals
                                          in
                                          let new_log = try
                                                (* let old_vals = Log.find name log in *)
                                                          let (old_vals, lev) = Log.find name log in
                                                          if is_reducible env (!!! arg_vals_refined) (!!! old_vals)
                                                            (* then None *)
                                                            then (Printf.printf "Level of divergence %d\n" lev; None)
                                                            (* else Some (Log.add name (!!! arg_vals_refined) log) *)
                                                            else Some (Log.add name (!!! arg_vals_refined, lev + 1) log)
                                                        (* with Not_found -> Some (Log.add name (!!! arg_vals_refined) log) *)
                                                        with Not_found -> Some (Log.add name (!!! arg_vals_refined, 0) log)
                                          in
                                          match new_log with
                                          | None      -> Diverge
                                          | Some log' -> let arg_names, body = Defs.find name defs in
                                                         let iset' = List.fold_left2 (fun is an av -> IdentSet.bind_ident an av is)
                                                                                     iset
                                                                                     arg_names
                                                                                     arg_vals_refined
                                                         in
                                                         match run_goal defs log' iset' body st with
                                                         | Result ss -> Result ss
                                                         | Diverge -> Diverge
                                                         | RebuildFunc body' -> let defs' = Defs.add name (arg_names, body') defs
                                                                                in (if Log.mem name log
                                                                                        then run_goal defs' log iset (Invoke (name, arg_vals)) st (* Restart (name, defs') *)
                                                                                        else run_goal defs' log iset (Invoke (name, arg_vals)) st)
                                                         | Restart (name, defs') -> (if Log.mem name log
                                                                                        then Restart (name, defs')
                                                                                        else run_goal defs' log iset (Invoke (name, arg_vals)) st)

                                                         (* process_result (fun ss -> Result ss)
                                                                        (* (fun body' -> Restart (Defs.add name (arg_names, body') defs)) *)
                                                                        (fun body' -> Restart (name, Defs.add name (arg_names, body') defs))
                                                                        (run_goal defs log' iset' body st) *)
      in
      let rec run_attempt : (term list * goal) Defs.t -> goal -> State.t -> State.t Stream.t = fun defs goal st ->
         match run_goal defs Log.empty iset goal st with
         | Result      ss    -> Stream.of_list ss
         | Diverge           -> raise Divergence
         | RebuildFunc goal' -> run_attempt defs  goal' st
         | Restart    (_, defs') -> run_attempt defs' goal  st
      in
      run_attempt defs goal

    type context =
    | Hole
    | ConjunctionS of context
    | ConjunctionL of (context * int) * (goal * int) list
    | ConjunctionR of (goal * int) * context
    | DisjunctionS of context
    | DisjunctionL of context * goal list
    | DisjunctionR of goal * context
    | Fresh        of term * context

    let rec print_context = function
    | Hole -> Printf.printf "[]"
    | ConjunctionS c -> Printf.printf "CS ("; print_context c; Printf.printf ")"
    | ConjunctionL ((c, _), _)  -> Printf.printf "CL ("; print_context c; Printf.printf ", _)"
    | ConjunctionR (_, c) -> Printf.printf "CR (_, "; print_context c; Printf.printf ")"
    | DisjunctionS c -> Printf.printf "DS ("; print_context c; Printf.printf ")"
    | DisjunctionL (c, _) -> Printf.printf "DL ("; print_context c; Printf.printf ", _)"
    | DisjunctionR (_, c) -> Printf.printf "DR (_, "; print_context c; Printf.printf ")"
    | Fresh (_, c) -> Printf.printf "FR ("; print_context c; Printf.printf ")"

    let rec plug_in_context g = function
    | Hole                          -> g
    | ConjunctionS cntxt            -> plug_in_context (Conjunction [(g, 0)]) cntxt (* Conjunction [(plug_in_context g cntxt, 0)] *)
    | ConjunctionL ((cntxt, i), cs) -> plug_in_context (Conjunction ((g, i) :: cs)) cntxt  (* Conjunction ((plug_in_context g cntxt, i) :: cs) *)
    | ConjunctionR (c, cntxt)       -> let Conjunction cs = g in plug_in_context (Conjunction (c :: cs)) cntxt (* let Conjunction cs = plug_in_context g cntxt in Conjunction (c :: cs) *)
    | DisjunctionS cntxt            -> plug_in_context (Disjunction [g]) cntxt (* Disjunction [plug_in_context g cntxt] *)
    | DisjunctionL (cntxt, gs)      -> plug_in_context (Disjunction (g :: gs)) cntxt   (* Disjunction (plug_in_context g cntxt :: gs) *)
    | DisjunctionR (hg, cntxt)      -> let Disjunction gs = g in plug_in_context (Disjunction (hg :: gs)) cntxt  (* let Disjunction gs = plug_in_context g cntxt in Disjunction (hg :: gs) *)
    | Fresh        (t, cntxt)       -> plug_in_context (Fresh (t, g)) cntxt (* Fresh (t, plug_in_context g cntxt) *)

    type cop = (string * term list * context) option

    let upd_cop : (context -> context) -> cop -> cop = fun f -> function
    | None -> None
    | Some (name, arg_names, c) -> Some (name, arg_names, f c)

    let rec restart_order : (goal * int) list -> (goal * int) list =
      let rec insert : 'a -> int -> 'a list -> 'a list = fun e i xs ->
        match i, xs with
        | 0, _      -> e :: xs
        | _, []     -> [e]
        | _, hd::tl -> hd :: insert e (i - 1) tl
      in function
      | []           -> []
      | (g, i) :: cs -> insert (g, 0) i (restart_order cs)

    let rec swap : 'a -> int -> 'a list -> ('a * 'a list) = fun e i xs ->
      match i, xs with
      | 0, r::tl -> (r, e::tl)
      | _, x::tl -> let (r, xs') = swap e (i - 1) tl in (r, x::xs')

    let rec run : bool -> (term list * goal) Defs.t -> cop -> term Log.t -> IdentSet.t -> goal -> State.t -> State.t SupportStream.t =
      fun mode defs cop log iset goal ((env, subst, constr) as st) ->
        match goal with
        | Unification (x, y)           -> run_unification_sup (logic_of_term @@ IdentSet.refine env iset x)
                                                              (logic_of_term @@ IdentSet.refine env iset y)
                                                              st
        | Disequality (x, y)           -> run_disequality_sup (logic_of_term @@ IdentSet.refine env iset x)
                                                              (logic_of_term @@ IdentSet.refine env iset y)
                                                              st
        (* | Marked       g               -> run mode defs cop log iset g st *)
        | Fresh       (x, g)           -> let var, env' = Env.fresh env in
                                          let new_iset = IdentSet.bind_ident x (term_of_logic var) iset in
                                          run mode defs (upd_cop (fun c -> Fresh (x, c)) cop) log new_iset g (env', subst, constr)
        | Disjunction [g]              -> run mode defs (upd_cop (fun c -> DisjunctionS c) cop) log iset g st
        | Disjunction (g :: gs)        -> let lss = run mode defs (upd_cop (fun c -> DisjunctionL (c, gs)) cop) log iset g st in
                                          let rss = run mode defs (upd_cop (fun c -> DisjunctionR (g, c)) cop) log iset (Disjunction gs) st in
                                          SupportStream.mplus_s lss rss
        | Conjunction [(g, _)]         -> SupportStream.start_node (run mode defs (upd_cop (fun c -> ConjunctionS c) cop) log iset g st)
        | Conjunction ((g, i) :: cs)   -> let lss = run mode defs (upd_cop (fun c -> ConjunctionL ((c, i), cs)) cop) log iset g st in
                                          let rssf = run mode defs (upd_cop (fun c -> ConjunctionR ((g, i), c)) cop) log iset (Conjunction cs) in
                                          let handler = SupportStream.from_fun (fun () ->
                                            if List.length cs <= i
                                            then SupportStream.divergence
                                            else
                                              let init_order = restart_order cs in
                                              let ((g', _), cs') = swap (g, 0) i init_order in
                                              let new_goal = Conjunction ((g', i + 1) :: cs') in
                                              let new_defs = match cop with
                                                             | None -> defs
                                                             | Some (name, arg_names, cntxt) -> Defs.add name (arg_names, ((*print_context cntxt; Printf.printf "\n";*) plug_in_context new_goal cntxt)) defs
                                              in
                                              run mode new_defs cop log iset new_goal st
                                          ) in
                                          SupportStream.catch (SupportStream.shift_first i (SupportStream.bind lss rssf)) handler
        | Invoke (name, arg_vals)      -> SupportStream.from_fun (fun () -> (** )Printf.printf "%s\n" name;( **)
                                            let arg_vals_refined = List.map (fun av -> term_of_logic (refine st (logic_of_term (IdentSet.refine env iset av)))) arg_vals
                                            in
                                            let new_log =
                                              if mode
                                              then
                                                try
                                                  let old_vals = Log.find name log in
                                                  if is_reducible env (!!! arg_vals_refined) (!!! old_vals)
                                                    then ((**)Printf.printf "Divergence in %s\n" name;(**) None)
                                                    else Some (Log.add name (!!! arg_vals_refined) log)
                                                with Not_found -> Some (Log.add name (!!! arg_vals_refined) log)
                                              else Some log
                                            in
                                            match new_log with
                                            | None      -> SupportStream.divergence
                                            | Some log' -> let arg_names, body = Defs.find name defs in
                                                           let iset' = List.fold_left2 (fun is an av -> IdentSet.bind_ident an av is)
                                                                                       iset
                                                                                       arg_names
                                                                                       arg_vals_refined
                                                           in
                                                           run mode defs (Some (name, arg_names, Hole)) log' iset' body st
                                         )

    let final_run (iset, defs, goal) st =
      let defs, goal = List.map (fun (name, (args, body)) -> (name, (args, pre_opt body))) defs, pre_opt goal in
      let defs = List.fold_left (fun acc (k, x) -> Defs.add k x acc) Defs.empty defs in
      Stream.filter (* (fun h -> SupportStream.print_his h; Printf.printf "\n\n\n%!") *) []
        (SupportStream.to_h_stream
          (run true defs None Log.empty iset goal st)
          (SupportStream.to_h_stream (run false defs None Log.empty iset goal st) Stream.nil))
  end


module ExtractDeepest =
  struct
    let ext2 x = x

    let succ prev (a, z) =
      let foo, base = prev z in
      ((a, foo), base)
  end

module ApplyTuple =
  struct
    let one arg x = x arg

    let succ prev = fun arg (x, y) -> (x arg, prev arg y)
  end

module ApplyLatest =
  struct
    let two = (ApplyTuple.one, ExtractDeepest.ext2)

    let apply (appf, extf) tup =
      let x, base = extf tup in
      appf base x

    let succ (appf, extf) = (ApplyTuple.succ appf, ExtractDeepest.succ extf)
  end

module Uncurry =
  struct
    let succ k f (x,y) = k (f x) y
  end

type 'a refiner = State.t Stream.t -> 'a logic Stream.t

let refiner : 'a logic -> 'a refiner = fun x ans ->
  Stream.map (fun st -> refine st x) ans

module LogicAdder =
  struct
    let zero f = (*Interpretation.run_prog*) Interpretation.final_run f

    let succ (prev: 'a -> State.t -> 'b) (f: 'c logic -> 'a) : State.t -> 'c refiner * 'b =
      call_fresh (fun logic st -> (refiner logic, prev (f logic) st))
  end

let one () = (fun x -> LogicAdder.(succ zero) x), (@@), ApplyLatest.two

let succ n () =
  let adder, currier, app = n () in
  (LogicAdder.succ adder, Uncurry.succ currier, ApplyLatest.succ app)

let two   () = succ one   ()
let three () = succ two   ()
let four  () = succ three ()
let five  () = succ four  ()

let q     = one
let qr    = two
let qrs   = three
let qrst  = four
let pqrst = five

let run n goalish f =
  let adder, currier, app_num = n () in
  let run f = f (State.empty ()) in
  run (adder goalish) |> ApplyLatest.apply app_num |> (currier f)
