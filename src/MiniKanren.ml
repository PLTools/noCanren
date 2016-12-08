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

open Printf

module Stream =
  struct

    type 'a t = Nil | Cons of 'a * 'a t | Lazy of 'a t Lazy.t

    let from_fun (f: unit -> 'a t) : 'a t = Lazy (Lazy.from_fun f)

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
           | Lazy  z      -> retrieve ~n (Lazy.force z)

    let take ?(n=(-1)) s = fst @@ retrieve ~n s

    let hd s = List.hd @@ take ~n:1 s
    let tl s = snd @@ retrieve ~n:1 s

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
    | Lazy s -> Lazy (Lazy.from_fun (fun () -> map f @@ Lazy.force s))

    let rec iter f = function
    | Nil -> ()
    | Cons (x, xs) -> f x; iter f xs
    | Lazy s -> iter f @@ Lazy.force s

  end

let (!!!) = Obj.magic;;
type w = Unboxed of Obj.t | Boxed of int * int * (int -> Obj.t) | Invalid of int

let rec wrap (x : Obj.t) =
  Obj.(
    let is_valid_tag =
      List.fold_left
      (fun f t tag -> tag <> t && f tag)
      (fun _ -> true)
      [lazy_tag   ; closure_tag  ; object_tag  ; infix_tag ;
       forward_tag; no_scan_tag  ; abstract_tag; custom_tag;
       custom_tag  ; unaligned_tag; out_of_heap_tag
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
    (* | Boxed (t,l,f) when t=0 && l=1 && (match wrap (f 0) with Unboxed i when !!!i >=10 -> true | _ -> false) ->
       Printf.bprintf b "var%d" (match wrap (f 0) with Unboxed i -> !!!i | _ -> failwith "shit") *)

    | Boxed   (t, l, f) ->
        Buffer.add_string b (Printf.sprintf "boxed %d <" t);
        for i = 0 to l - 1 do (inner (f i); if i<l-1 then Buffer.add_string b " ") done;
        Buffer.add_string b ">"
  in
  inner x;
  Buffer.contents b

type ('a, 'b, 'c) fancy = 'a * (('a->bool) -> 'a -> 'c);;
type 'a logic = 'a;;

class type virtual ['a, 'ia, 'sa, 'inh, 'syn] logic_tt =
  object
    method value :
      'inh -> ('inh, 'a logic, 'syn, < a : 'ia -> 'a -> 'sa >) GT.a ->
        ('ia, 'a, 'sa, < a : 'ia -> 'a -> 'sa >) GT.a -> 'syn
    method t_logic : ('ia -> 'a -> 'sa) -> 'inh -> 'a logic -> 'syn
  end
let (logic :
 (('ia -> 'a -> 'sa) -> ('a, 'ia, 'sa, 'inh, 'syn) #logic_tt -> 'inh ->
   'a logic -> 'syn, unit)
   GT.t) =
  let rec logic_gcata fa trans inh subj =
    let rec self = logic_gcata fa trans
    and tpo = object method a = fa end in
    match subj with
      p0 ->
        trans#value inh (GT.make self subj tpo)
          (let (e0) = p0 in GT.make fa e0 tpo)
  in
  {GT.gcata = logic_gcata; GT.plugins = ()}
class virtual ['a, 'ia, 'sa, 'inh, 'syn] logic_t =
  object (this)
    method virtual value :
      'inh -> ('inh, 'a logic, 'syn, < a : 'ia -> 'a -> 'sa >) GT.a ->
        ('ia, 'a, 'sa, < a : 'ia -> 'a -> 'sa >) GT.a -> 'syn
    method t_logic fa = GT.transform logic fa this
  end
class type ['a] show_logic_env_tt = object  end
class type ['a, 'sa] gmap_logic_env_tt = object  end
class ['a] show_proto_logic env =
  object (this)
    inherit ['a, unit, string, unit, string] logic_t
    method value inh subj p0 =
      ("(" ^ (let (e0) = p0 in ("(" ^ e0.GT.fx ()) ^ ")")) ^ ")"
  end
class ['a, 'sa] gmap_proto_logic env =
  object (this)
    inherit ['a, unit, 'sa, unit, 'sa logic] logic_t
    method value inh subj p0 = (let (e0) = p0 in e0.GT.fx ())
  end
class ['a] show_logic_t =
  let self = Obj.magic (ref ()) in
  object (this)
    inherit ['a, unit, string, unit, string] logic_t
    inherit ['a] show_proto_logic self
    initializer (:=) self (this :> 'a show_logic_t)
  end
class ['a, 'sa] gmap_logic_t =
  let self = Obj.magic (ref ()) in
  object (this)
    inherit ['a, unit, 'sa, unit, 'sa logic] logic_t
    inherit ['a, 'sa] gmap_proto_logic self
    initializer (:=) self (this :> ('a, 'sa) gmap_logic_t)
  end
let (logic :
 (('ia -> 'a -> 'sa) -> ('a, 'ia, 'sa, 'inh, 'syn) #logic_tt -> 'inh ->
   'a logic -> 'syn, < show : ('a -> string) -> 'a logic -> string;
 gmap : ('a -> 'sa) -> 'a logic -> 'sa logic >)
   GT.t) =
  {GT.gcata = logic.GT.gcata;
   GT.plugins =
     object
       method show a = GT.transform logic (GT.lift a) (new show_logic_t) ()
       method gmap a = GT.transform logic (GT.lift a) (new gmap_logic_t) ()
     end}
;;

(* N.B. internally Obj.repr : 'a -> Obj.t = "%idefntity" *)
(* exception DelayedRefinement of ((Obj.t -> bool) -> Obj.t -> Obj.t) * Obj.t;; *)
exception DelayedRefinement of Obj.t * Obj.t
let delay f x = raise (DelayedRefinement (Obj.repr f, Obj.repr x));;

@type 'a unlogic = | Var of GT.int GT.list * GT.int * 'a logic GT.list
                   | Value of 'a
                   with show;;
let discr : ('a->bool) -> 'a -> 'c =
  fun is_logic x ->
    let () = printf "Discr: %s\n%!" (generic_show x) in
    if is_logic x then Obj.magic x
    else
      let () = printf "return from discr with Value '%s'\n%!" (generic_show x) in
      Obj.magic @@ Value x
;;
(* let (_:int) = discr;; *)
let lift: 'a -> ('a, 'a, 'a) fancy = fun x -> (x,(fun _ y -> printf "id with '%s'\n%!" (generic_show y); y))

let inj: ('a, 'b, 'c) fancy -> ('a, 'b logic, 'c unlogic) fancy =
  fun (a,f) ->
    let () = printf "Inside inj for a = '%s'\n%!" (generic_show a) in
    let new_r cond x =
      let () = printf "We got into new_r with x = %s\n%!" (generic_show x) in
      delay (fun x -> discr !!!cond @@ f cond x) x
    in
    printf "new R = '%s' with address %d\n%!" (generic_show a) (2*(Obj.magic new_r));
    (a, new_r)

let (!!) = inj

(*
@type 'a logic = Var of GT.int GT.list * GT.int * 'a logic GT.list | Value of 'a with show, html, eq, compare, foldl, foldr, gmap

let logic = {logic with
  gcata = ();
  plugins =
    object
      method html    = logic.plugins#html
      method eq      = logic.plugins#eq
      method compare = logic.plugins#compare
      method foldr   = logic.plugins#foldr
      method foldl   = logic.plugins#foldl
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
*)

exception Not_a_value

(*
let prj_k k = function Value x -> x | Var (_, i, c) -> k i c
let prj x = prj_k (fun _ -> raise Not_a_value) x

let (!?) = prj
*)
exception Occurs_check

module Env :
  sig
    type t

    val empty  : unit -> t
    val fresh  : t -> 'a logic * t
    val var    : t -> 'a logic -> int option
    val is_var : t -> 'a logic -> bool
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
    let is_var env v = None <> var env v
  end

let fst3 (x,_,_) = x
let snd3 (_,x,_) = x
let trd3 (_,_,x) = x

module Subst :
  sig
    type t

    val empty   : t

    (* Screw this.  We need to do (int * (...stuff...)) to save on boxing *)
    val of_list : (int * Obj.t * Obj.t * (Obj.t option)) list -> t
    val split   : t -> Obj.t list * Obj.t list
    val walk    : Env.t -> 'a logic -> t -> 'a logic
    val unify   : Env.t -> 'a logic -> 'a logic -> Obj.t option -> t option -> (int * Obj.t * Obj.t * (Obj.t option)) list * t option
    val show    : t -> string
  end =
  struct
    module M = Map.Make (struct type t = int let compare = Pervasives.compare end)
    (* map from var indicies to tuples of (actual vars, value, reifier_func) *)
    type t = (Obj.t * Obj.t * Obj.t) M.t

    let show m = (M.fold (fun i (_, x, _) s -> s ^ Printf.sprintf "%d -> %s; " i (generic_show x)) m "subst {") ^ "}"

    let empty = M.empty

    let of_list l = List.fold_left (fun s (i, v, t, f) -> M.add i Obj.(repr v, repr t, repr f) s) empty l

    let split s = M.fold (fun _ (x, t, _func) (xs, ts) -> Obj.(repr x)::xs, Obj.(repr t)::ts) s ([], [])

    let rec walk env var subst =
      match Env.var env var with
      | None   -> var
      | Some i ->
          try walk env (snd3 (M.find i (!!! subst))) subst with Not_found -> var

    let rec occurs env xi term subst =
      let y = walk env term subst in
      match Env.var env y with
      | Some yi -> xi = yi
      | None ->
         let wy = wrap (Obj.repr y) in
         match wy with
         | Invalid 247
         | Unboxed _ -> false
         | Invalid n -> invalid_arg (Printf.sprintf "Invalid value in occurs check (%d)" n)
         | Boxed (_, s, f) ->
            let rec inner i =
              if i >= s then false
              else occurs env xi (!!!(f i)) subst || inner (i+1)
            in
            inner 0

    let unify env x y f subst =
      let extend xi x term f delta subst =
        if occurs env xi term subst then raise Occurs_check
        else (xi, !!!x, !!!term, !!!f)::delta, Some (!!! (M.add xi (!!!x, term, !!!f) (!!! subst)))
      in
      let rec unify x y f (delta, subst) =
        match subst with
        | None -> delta, None
        | (Some subst) as s ->
            let x, y = walk env x subst, walk env y subst in
            match Env.var env x, Env.var env y with
            | Some xi, Some yi -> if xi = yi then delta, s else extend xi x y f delta subst
            | Some xi, _       ->
              let () = printf "unifying var %d with '%s'\n%!" xi (generic_show y) in
              extend xi x y f delta subst
            | _      , Some yi ->
              let () = printf "unifying '%s' with var %d\n%!" (generic_show x) yi in
              extend yi y x f delta subst
            | _ ->
                let wx, wy = wrap (Obj.repr x), wrap (Obj.repr y) in
                (match wx, wy with
                 (* | Invalid 247, Invalid 247 -> delta,s *)
                 | Unboxed vx, Unboxed vy -> if vx = vy then delta, s else delta, None
                 | Boxed (tx, sx, fx), Boxed (ty, sy, fy) ->
                    if tx = ty && sx = sy
                    then
                      let rec inner i (delta, subst) =
                      match subst with
                        | None -> delta, None
                        | Some _ ->
                          if i < sx
                          then inner (i+1) (unify (!!!(fx i)) (!!!(fy i)) f (delta, subst))
                          else delta, subst
                      in
                      inner 0 (delta, s)
                    else delta, None
                 | Invalid n, _
                 | _, Invalid n -> invalid_arg (Printf.sprintf "Invalid values for unification (%d)" n)
                 | _ -> delta, None
                )
      in
      unify x y f ([], subst)

  end

module State =
  struct
    type t = Env.t * Subst.t * Subst.t list
    let empty () = (Env.empty (), Subst.empty, [])
    let env   (env, _, _) = env
    let show  (env, subst, constr) = Printf.sprintf "st {%s, %s}" (Subst.show subst) (GT.show(GT.list) Subst.show constr)
  end

type goal = State.t -> State.t Stream.t

type ('a, 'b) fancier = ('a, 'b logic, 'b unlogic) fancy

let dummy_discr _ x = !!!x

let call_fresh f (env, subst, constr) =
  let x, env' = Env.fresh env in
  f (x, !!!dummy_discr) (env', subst, constr)

exception Disequality_violated

let (===) x y (env, subst, constr) =
  let () = printf "(===) '%s' and '%s'\n%!" (generic_show x) (generic_show y) in
  (* we should always unify two fancy types *)
  assert Obj.(tag  @@ repr x = 0);
  assert Obj.(tag  @@ repr y = 0);
  assert Obj.(size @@ repr x = 2);
  assert Obj.(size @@ repr y = 2);
  assert Obj.(tag @@ repr @@ snd x = closure_tag);
  assert Obj.(tag @@ repr @@ snd y = closure_tag);
  let () =
    let foo x y =
      if Env.is_var env (fst !!!x) && not (Env.is_var env (fst !!!y))
      then Obj.(set_field (repr x) 1 (field (repr y) 1))
    in
    foo x y;
    foo y x;
  in
  let (x,f) = x in
  let (y,_) = y in

  try
    let prefix, subst' = Subst.unify env x y (Some (!!!f)) (Some subst) in
    begin match subst' with
    | None -> Stream.nil
    | Some s ->
        try
          (* TODO: only apply constraints with the relevant vars *)
          let constr' =
            List.fold_left (fun css' cs ->
              let x,t = Subst.split cs in
              try
                let p, s' = Subst.unify env (!!!x) (!!!t) (Some (Obj.repr f)) subst' in
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
(*
let (=/=) x y ((env, subst, constr) as st) =
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
*)
let conj f g st = Stream.bind (f st) g

let (&&&) = conj

let disj f g st = Stream.mplus (f st) (g st)

let (|||) = disj

let rec (?|) = function
| [h]  -> h
| h::t -> h ||| ?| t

let rec (?&) = function
| [h]  -> h
| h::t -> h &&& ?& t

let conde = (?|)

module Fresh =
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

  end

let success st = Stream.cons st Stream.nil
let failure _  = Stream.nil

(* let eqo x y t =
  conde [
    (x === y) &&& (t === inj@@lift true);
    (x =/= y) &&& (t === inj@@lift false);
  ]

let neqo x y t =
  conde [
    (x =/= y) &&& (t === inj@@lift true);
    (x === y) &&& (t === inj@@lift false);
  ];; *)

(* @type ('a, 'l) llist = Nil | Cons of 'a * 'l with show, html, eq, compare, foldl, foldr, gmap
@type 'a lnat = O | S of 'a with show, html, eq, compare, foldl, foldr, gmap *)

module type T = sig
  type 'a t
  val fmap : ('a -> 'b) -> 'a t -> 'b t
end
module type T2 = sig
  type ('a, 'b) t
  val fmap : ('a -> 'c) -> ('b -> 'd) -> ('a, 'b) t -> ('c, 'd) t
end

module Fmap1 (T : T) = struct
  open T
  (* external fmap : ('a, 'b, 'c) fancy t -> ('a t, 'b t, 'c t) fancy = "%identity" *)
  let fmap : ('a, 'b, 'c) fancy t -> ('a t, 'b t, 'c t) fancy =
    (fun x ->
      Obj.magic begin
      let fi : (('a1->bool) -> 'a1 -> 'c1) ref = ref (fun _  _ -> assert false) in
      let left = T.fmap (fun (z,f) -> fi := f; z) !!!x in
      (* printf "left = '%s'\n%!" (generic_show left); *)
      let right cond y =
        (* printf "Inside right: right is '%s' with address = %d\n%!" (generic_show right) (2 * (!!!right));
        printf "Inside right: cond is '%s' with address = %d\n%!" (generic_show cond) (2 * (!!!cond));
        printf "Inside right: y    is '%s'\n%!" (generic_show y); *)
        (*discr cond @@*) T.fmap (fun heck ->
          (* printf "Inside T.fmap, heck = '%s'\n%!" (generic_show heck); *)
          !fi cond heck
        ) y
      in
      (* printf "Right is '%s' with address = %d\n%!" (generic_show right) (2 * (!!!right)); *)
      (left, right)
      end
    )
end

module Fmap2 (T : T2) = struct
  type ('a,'b) t = ('a,'b) T.t
  let fmap : (('a, 'b, 'c) fancy, ('q, 'w, 'e) fancy) t ->
                   (('a, 'q) t, ('b, 'w) t, ('c, 'e) t) fancy = fun x ->
    Obj.magic begin
      let () = print_endline "inside Fmap2.fmap" in
      let fi1 : (('a1->bool) -> 'a1 -> 'c1) ref = ref (fun _  _ -> assert false) in
      let fi2 : (('a2->bool) -> 'a2 -> 'c2) ref = ref (fun _  _ -> assert false) in
      let left = T.fmap (fun (z,f) -> fi1 := f; z) (fun (z,f) -> fi2 := f; z) !!!x in
      let right cond y = T.fmap (!fi1 cond) (!fi2 cond) y in
      (left,right)
    end
end


(* module Higher = struct
  (** Type expression application. *)
  type ('p, 'f) app

  (** Construct a newtype for a type constructor with no parameters. *)
  module type Newtype0 = sig
    type s
    type t
    external inj : s -> t = "%identity"
    external prj : t -> s = "%identity"
  end

  (** Construct a newtype for a type constructor with one parameter. *)
  module type Newtype1 = sig
    type 'a s
    type t
    external inj : 'a s -> ('a, t) app = "%identity"
    external prj : ('a, t) app -> 'a s = "%identity"

    (* external fancify1 :
      (('a, t) app -> 'a s) -> (('a,t) app, 'b) fancy -> ('a s, 'b) fancy = fun _ -> "%identity"
    external fancify2 :
      (('a, t) app -> 'a s) -> ('b, ('a,t) app) fancy -> ('b, 'a s) fancy = fun _ -> "%identity" *)

    (* funky is fancify1 and fancify2 together for this concrete module *)
    external funky : (('a, t) app, ('b, t) app) fancy -> ('a s, 'b s) fancy = "%identity"
  end

  (** Construct a newtype for a type constructor with two parameters. *)
  module type Newtype2 = sig
    type ('a, 'b) s
    type t
    external inj : ('a, 'b) s -> ('a, ('b, t) app) app = "%identity"
    external prj : ('a, ('b, t) app) app -> ('a, 'b) s = "%identity"
  end

  module Common = struct
    type t
    external inj : 'a -> 'b = "%identity"
    external prj : 'a -> 'b = "%identity"
  end

  module Newtype0 (T : sig type t end) : Newtype0 with type s = T.t = struct
    type s = T.t
    include Common
  end

  module Newtype1 (T : sig type 'a t end) : Newtype1 with type 'a s = 'a T.t = struct
    type 'a s = 'a T.t
    include Common
    (* external fancify1 :
      (('a, t) app -> 'a s) -> (('a,t) app, 'b) fancy -> ('a s, 'b) fancy = "%identity"
    external fancify2 :
      (('a, t) app -> 'a s) -> ('b, ('a,t) app) fancy -> ('b, 'a s) fancy = "%identity" *)
    external funky : (('a, t) app, ('b, t) app) fancy -> ('a s, 'b s) fancy = "%identity"
  end

  module Newtype2 (T : sig type ('a, 'b) t end) = struct
    type ('a, 'b) s = ('a, 'b) T.t
    include Common
  end


  let fmap1 : (('a, 'b) fancy, 't) app -> (('a, 't)app, ('b, 't)app) fancy = fun x -> x

  (* let prj_fancy (p1: 'a -> 'c) -> (p2: 'a -> 'c) -> ('a,') *)

end *)

(* let lmap : ('a, 'b) fancy -> (('a, 'l) llist as 'l, ('b, 'm) llist as 'm) fancy = fun x -> Cons (x, Nil)

let cons : ('a, 'b logic) fancy -> (('a, 'z) llist as 'z, ('b logic, 'c) llist logic as 'c) fancy -> (('a, 'z) llist, ('b logic, 'c) llist logic) fancy = fun x y ->
  Cons (x, y)

let nil : (('a, 'z) llist as 'z, ('a logic, 'c) llist logic as 'c) fancy = Nil *)

(*
module Bool =
  struct

    type 'a logic' = 'a logic
    let logic' = logic

    type ground = bool

    let ground = {
      GT.gcata = ();
      GT.plugins =
        object(this)
          method html    n   = GT.html   (GT.bool) n
          method eq      n m = GT.eq     (GT.bool) n m
          method compare n m = GT.compare(GT.bool) n m
          method foldr   n   = GT.foldr  (GT.bool) n
          method foldl   n   = GT.foldl  (GT.bool) n
          method gmap    n   = GT.gmap   (GT.bool) n
          method show    n   = GT.show   (GT.bool) n
        end
    }

    type logic = bool logic'

    let logic = {
      GT.gcata = ();
      GT.plugins =
        object(this)
          method html    n   = GT.html   (logic') (GT.html   (ground)) n
          method eq      n m = GT.eq     (logic') (GT.eq     (ground)) n m
          method compare n m = GT.compare(logic') (GT.compare(ground)) n m
          method foldr   a n = GT.foldr  (logic') (GT.foldr  (ground)) a n
          method foldl   a n = GT.foldl  (logic') (GT.foldl  (ground)) a n
          method gmap    n   = GT.gmap   (logic') (GT.gmap   (ground)) n
          method show    n   = GT.show   (logic') (GT.show   (ground)) n
        end
    }

    let (!) = (!!)

    let (|^) a b c =
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
    let (||) a b = oro  a b !true

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
          method html    n = GT.html   (lnat) this#html    n
          method eq      n = GT.eq     (lnat) this#eq      n
          method compare n = GT.compare(lnat) this#compare n
          method foldr   n = GT.foldr  (lnat) this#foldr   n
          method foldl   n = GT.foldl  (lnat) this#foldl   n
          method gmap    n = GT.gmap   (lnat) this#gmap    n
          method show    n = GT.show   (lnat) this#show    n
        end
    }

    type logic  = logic t logic'

    let logic = {
      GT.gcata = ();
      GT.plugins =
        object(this)
          method html    n   = GT.html   (logic') (GT.html   (lnat) this#html   ) n
          method eq      n m = GT.eq     (logic') (GT.eq     (lnat) this#eq     ) n m
          method compare n m = GT.compare(logic') (GT.compare(lnat) this#compare) n m
          method foldr   a n = GT.foldr  (logic') (GT.foldr  (lnat) this#foldr  ) a n
          method foldl   a n = GT.foldl  (logic') (GT.foldl  (lnat) this#foldl  ) a n
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

    let rec addo x y z =
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
    let (<) x y = lto x y !true

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
          method html    fa l = GT.html   (llist) fa (this#html    fa) l
          method eq      fa l = GT.eq     (llist) fa (this#eq      fa) l
          method compare fa l = GT.compare(llist) fa (this#compare fa) l
          method foldr   fa l = GT.foldr  (llist) fa (this#foldr   fa) l
          method foldl   fa l = GT.foldl  (llist) fa (this#foldl   fa) l
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
          method html    fa l   = GT.html   (logic') (GT.html   (llist) fa (this#html    fa)) l
          method eq      fa a b = GT.eq     (logic') (GT.eq     (llist) fa (this#eq      fa)) a b
          method compare fa a b = GT.compare(logic') (GT.compare(llist) fa (this#compare fa)) a b
          method foldr   fa a l = GT.foldr  (logic') (GT.foldr  (llist) fa (this#foldr   fa)) a l
          method foldl   fa a l = GT.foldl  (logic') (GT.foldl  (llist) fa (this#foldl   fa)) a l
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

    let rec foldro f a xs r =
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
                 (f z a1) &&&
                 (mapo f zs a2) &&&
                 (ys === a1 % a2)
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
      )
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
*)

let rec refine : State.t -> ('a, 'b, 'c) fancy -> 'c
  = fun ((e, s, c) as st) (x,func) ->
  let rec walk' recursive env var subst =
    let () = printf "walk' for var = '%s'\n%!" (generic_show var) in
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
           (Obj.magic var)
         | Invalid n -> invalid_arg (sprintf "Invalid value for reconstruction (%d)" n)
        )
    | Some i when recursive ->
      (var : _ unlogic)
      (* invalid_arg "Free variable in refine." *)
(*
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
*)
    | _ ->
      let () = printf "Got a value '%s'\n%!" (generic_show var) in
      (Obj.magic var)
  in
  let () = printf "going to refine....\n%!" in
  let pizda = !!!(walk' true e (!!!x) s)in
  printf "PIZDA\n%!";
  printf "   x  is '%s' with address = %d\n%!" (generic_show x)     (2 * (!!!x));
  printf "func  is '%s' with address = %d\n%!" (generic_show func)  (2 * (!!!func));
  printf "pizda is '%s' with address = %d\n%!" (generic_show pizda) (2 * (!!!pizda));

  (* let ans =
    match pizda with
    | Var _ -> pizda
    | Value x -> Value (func (fun x ->
          printf "calling isVar of '%s' \n%!" (generic_show x);
          Env.var e x <> None)
          x)
    | _ -> failwith "Neither Var nor Value"
  in
  !!!ans *)
  (* let () =
    let zzz = !!!func (fun _ -> print_endline "HERR"; false) 5 in
    printf "zzz is '%s'\n%!" (generic_show zzz)
  in *)
  func (fun x ->
      let ans = Env.var e x <> None in
      printf "calling isVar of '%s' says %b\n%!" (generic_show x) ans;
      ans)
      pizda

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

type 'a refiner = State.t Stream.t -> 'a unlogic Stream.t

let refiner : ('a, 'b logic, 'b unlogic) fancy -> 'b refiner = fun x ans ->
  Stream.map (fun st -> refine st x) ans

module LogicAdder =
  struct
    let zero f = f

    let succ (prev: 'a -> State.t -> 'b) (f: ('c, 'z) fancier -> 'a) : State.t -> 'z refiner * 'b =
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
