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
    type 'a t =
        Nil
      | Cons of 'a * 'a t
      | Lazy of 'a t Lazy.t
    let from_fun (f : unit -> 'a t) : 'a t = Lazy (Lazy.from_fun f)
    let nil = Nil
    let cons h t = Cons (h, t)
    let rec is_empty =
      function
        Nil -> true
      | Lazy s -> (@@) is_empty (Lazy.force s)
      | _ -> false
    let rec retrieve ?(n = -1) s =
      if n = 0 then [], s
      else
        match s with
          Nil -> [], s
        | Cons (x, xs) ->
            let (xs', s') = retrieve ~n:(n - 1) xs in x :: xs', s'
        | Lazy z -> retrieve ~n (Lazy.force z)
    let take ?(n = -1) s = (@@) fst (retrieve ~n s)
    let hd s = (@@) List.hd (take ~n:1 s)
    let tl s = (@@) snd (retrieve ~n:1 s)
    let rec mplus fs gs =
      match fs with
        Nil -> gs
      | Cons (hd, tl) -> (@@) (cons hd) (from_fun (fun () -> mplus gs tl))
      | Lazy z -> from_fun (fun () -> mplus gs (Lazy.force z))
    let rec bind xs f =
      match xs with
        Cons (x, xs) -> from_fun (fun () -> mplus (f x) (bind xs f))
      | Nil -> nil
      | Lazy z -> from_fun (fun () -> bind (Lazy.force z) f)
    let rec map f =
      function
        Nil -> Nil
      | Cons (x, xs) -> Cons (f x, map f xs)
      | Lazy s -> Lazy (Lazy.from_fun (fun () -> (@@) (map f) (Lazy.force s)))
    let rec iter f =
      function
        Nil -> ()
      | Cons (x, xs) -> f x; iter f xs
      | Lazy s -> (@@) (iter f) (Lazy.force s)
    let rec zip fs gs =
      match fs, gs with
        Nil, Nil -> Nil
      | Cons (x, xs), Cons (y, ys) -> Cons ((x, y), zip xs ys)
      | _, Lazy s -> Lazy (Lazy.from_fun (fun () -> zip fs (Lazy.force s)))
      | Lazy s, _ -> Lazy (Lazy.from_fun (fun () -> zip (Lazy.force s) gs))
      | Nil, _ | _, Nil -> invalid_arg "streams have different lengths"
  end

let (!!!) = Obj.magic
type w =
    Unboxed of Obj.t
  | Boxed of int * int * (int -> Obj.t)
  | Invalid of int

let is_valid_tag t =
  let open Obj in
  not (List.mem t
    [lazy_tag; closure_tag; object_tag; infix_tag; forward_tag; no_scan_tag;
     abstract_tag; custom_tag; custom_tag; unaligned_tag; out_of_heap_tag])

let rec wrap (x : Obj.t) =
  Obj.(
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
      Invalid n -> Buffer.add_string b (Printf.sprintf "<invalid %d>" n)
    | Unboxed s when Obj.(string_tag = (@@) tag (repr s)) ->
        bprintf b "\"%s\"" ((!!!) s)
    | Unboxed n when (!!!) n = 0 -> Buffer.add_string b "[]"
    | Unboxed n -> Buffer.add_string b (Printf.sprintf "int<%d>" ((!!!) n))
    | Boxed (t, l, f) ->
        Buffer.add_string b (Printf.sprintf "boxed %d <" t);
        for i = 0 to l - 1 do
          inner (f i);
          if i < l - 1 then Buffer.add_string b " "
        done;
        Buffer.add_string b ">"
  in
  inner x; Buffer.contents b
type 'a logic =
    Var of int * 'a logic list
  | Value of 'a [@@deriving show { with_path = false }]
class type virtual ['a, 'ia, 'sa, 'inh, 'syn] logic_tt =
  object
    method c_Var :
      'inh -> ('inh, 'a logic, 'syn, < a : 'ia -> 'a -> 'sa >) GT.a ->
        GT.int -> 'a logic GT.list -> 'syn
    method c_Value :
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
      Var (p0, p1) -> trans#c_Var inh (GT.make self subj tpo) p0 p1
    | Value p0 ->
        trans#c_Value inh (GT.make self subj tpo) (GT.make fa p0 tpo)
  in
  {GT.gcata = logic_gcata; GT.plugins = ()}
class virtual ['a, 'ia, 'sa, 'inh, 'syn] logic_t =
  object (this)
    method virtual c_Var :
      'inh -> ('inh, 'a logic, 'syn, < a : 'ia -> 'a -> 'sa >) GT.a ->
        GT.int -> 'a logic GT.list -> 'syn
    method virtual c_Value :
      'inh -> ('inh, 'a logic, 'syn, < a : 'ia -> 'a -> 'sa >) GT.a ->
        ('ia, 'a, 'sa, < a : 'ia -> 'a -> 'sa >) GT.a -> 'syn
    method t_logic fa = GT.transform logic fa this
  end
class type ['a] show_logic_env_tt = object  end
class type ['a, 'sa] gmap_logic_env_tt = object  end
class type ['a] html_logic_env_tt = object  end
class type ['a] eq_logic_env_tt = object  end
class type ['a] compare_logic_env_tt = object  end
class type ['a, 'syn] foldl_logic_env_tt = object  end
class type ['a, 'syn] foldr_logic_env_tt = object  end
class ['a] show_proto_logic env =
  object (this)
    inherit ['a, unit, string, unit, string] logic_t
    method c_Value inh subj p0 = ("Value (" ^ p0.GT.fx ()) ^ ")"
    method c_Var inh subj p0 p1 =
      (("Var (" ^ GT.lift GT.int.GT.plugins#show () p0) ^ ", " ^
       GT.lift
         (GT.list.GT.plugins#show (GT.transform logic subj.GT.t#a this ())) ()
         p1) ^
      ")"
  end
class ['a, 'sa] gmap_proto_logic env =
  object (this)
    inherit ['a, unit, 'sa, unit, 'sa logic] logic_t
    method c_Value inh subj p0 = Value (p0.GT.fx ())
    method c_Var inh subj p0 p1 =
      Var
        (GT.lift GT.int.GT.plugins#gmap () p0,
         GT.lift
           (GT.list.GT.plugins#gmap (GT.transform logic subj.GT.t#a this ()))
           () p1)
  end
class ['a] html_proto_logic env =
  object (this)
    inherit ['a, unit, HTML.viewer, unit, HTML.viewer] logic_t
    method c_Value inh subj p0 =
      View.concat (HTML.b (HTML.string (this#cname "Value")))
        (HTML.ul (View.concat View.empty (p0.GT.fx ())))
    method c_Var inh subj p0 p1 =
      View.concat (HTML.b (HTML.string (this#cname "Var")))
        (HTML.ul
           (View.concat
              (View.concat View.empty
                 (HTML.li ~attrs:"" (GT.lift GT.int.GT.plugins#html () p0)))
              (HTML.li ~attrs:""
                 (GT.lift
                    (GT.list.GT.plugins#html
                       (GT.transform logic subj.GT.t#a this ()))
                    () p1))))
    method attribute : 'a logic -> string = fun _ -> ""
    method cname : string -> string = fun s -> s
  end
class ['a] eq_proto_logic env =
  object (this)
    inherit ['a, 'a, bool, 'a logic, bool] logic_t
    method c_Value inh subj p0 =
      match inh with
        Value p0_ -> true && p0.GT.fx p0_
      | _ -> false
    method c_Var inh subj p0 p1 =
      match inh with
        Var (p0_, p1_) ->
          (true && GT.int.GT.plugins#eq p0_ p0) &&
          GT.list.GT.plugins#eq (GT.transform logic subj.GT.t#a this) p1_ p1
      | _ -> false
  end
class ['a] compare_proto_logic env =
  object (this)
    inherit ['a, 'a, GT.comparison, 'a logic, GT.comparison] logic_t
    method c_Value inh subj p0 =
      match inh with
        Value p0_ -> GT.chain_compare GT.EQ (fun _ -> p0.GT.fx p0_)
      | other -> GT.compare_vari other subj.GT.x
    method c_Var inh subj p0 p1 =
      match inh with
        Var (p0_, p1_) ->
          GT.chain_compare
            (GT.chain_compare GT.EQ
               (fun _ -> GT.int.GT.plugins#compare p0_ p0))
            (fun _ ->
               GT.list.GT.plugins#compare
                 (GT.transform logic subj.GT.t#a this) p1_ p1)
      | other -> GT.compare_vari other subj.GT.x
  end
class ['a, 'syn] foldl_proto_logic env =
  object (this)
    inherit ['a, 'syn, 'syn, 'syn, 'syn] logic_t
    method c_Value inh subj p0 = p0.GT.fx inh
    method c_Var inh subj p0 p1 =
      GT.list.GT.plugins#foldl (GT.transform logic subj.GT.t#a this)
        (GT.int.GT.plugins#foldl inh p0) p1
  end
class ['a, 'syn] foldr_proto_logic env =
  object (this)
    inherit ['a, 'syn, 'syn, 'syn, 'syn] logic_t
    method c_Value inh subj p0 = p0.GT.fx inh
    method c_Var inh subj p0 p1 =
      GT.int.GT.plugins#foldr
        (GT.list.GT.plugins#foldr (GT.transform logic subj.GT.t#a this) inh
           p1)
        p0
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
class ['a] html_logic_t =
  let self = Obj.magic (ref ()) in
  object (this)
    inherit ['a, unit, HTML.viewer, unit, HTML.viewer] logic_t
    inherit ['a] html_proto_logic self
    initializer (:=) self (this :> 'a html_logic_t)
    method attribute : 'a logic -> string = fun _ -> ""
    method cname : string -> string = fun s -> s
  end
class ['a] eq_logic_t =
  let self = Obj.magic (ref ()) in
  object (this)
    inherit ['a, 'a, bool, 'a logic, bool] logic_t
    inherit ['a] eq_proto_logic self
    initializer (:=) self (this :> 'a eq_logic_t)
  end
class ['a] compare_logic_t =
  let self = Obj.magic (ref ()) in
  object (this)
    inherit ['a, 'a, GT.comparison, 'a logic, GT.comparison] logic_t
    inherit ['a] compare_proto_logic self
    initializer (:=) self (this :> 'a compare_logic_t)
  end
class ['a, 'syn] foldl_logic_t =
  let self = Obj.magic (ref ()) in
  object (this)
    inherit ['a, 'syn, 'syn, 'syn, 'syn] logic_t
    inherit ['a, 'syn] foldl_proto_logic self
    initializer (:=) self (this :> ('a, 'syn) foldl_logic_t)
  end
class ['a, 'syn] foldr_logic_t =
  let self = Obj.magic (ref ()) in
  object (this)
    inherit ['a, 'syn, 'syn, 'syn, 'syn] logic_t
    inherit ['a, 'syn] foldr_proto_logic self
    initializer (:=) self (this :> ('a, 'syn) foldr_logic_t)
  end
let (logic :
 (('ia -> 'a -> 'sa) -> ('a, 'ia, 'sa, 'inh, 'syn) #logic_tt -> 'inh ->
   'a logic -> 'syn, < show : ('a -> string) -> 'a logic -> string;
 gmap : ('a -> 'sa) -> 'a logic -> 'sa logic;
 html : ('a -> HTML.viewer) -> 'a logic -> HTML.viewer;
 eq : ('a -> 'a -> bool) -> 'a logic -> 'a logic -> bool;
 compare :
   ('a -> 'a -> GT.comparison) -> 'a logic -> 'a logic -> GT.comparison;
 foldl : ('syn -> 'a -> 'syn) -> 'syn -> 'a logic -> 'syn;
 foldr : ('syn -> 'a -> 'syn) -> 'syn -> 'a logic -> 'syn >)
   GT.t) =
  {GT.gcata = logic.GT.gcata;
   GT.plugins =
     object
       method show a = GT.transform logic (GT.lift a) (new show_logic_t) ()
       method gmap a = GT.transform logic (GT.lift a) (new gmap_logic_t) ()
       method html a = GT.transform logic (GT.lift a) (new html_logic_t) ()
       method eq a = GT.transform logic a (new eq_logic_t)
       method compare a = GT.transform logic a (new compare_logic_t)
       method foldl a = GT.transform logic a (new foldl_logic_t)
       method foldr a = GT.transform logic a (new foldr_logic_t)
     end}

(* miniKanren-related stuff starts here *)
type ('a, 'b) fancy = 'a

external lift : 'a -> ('a, 'a) fancy = "%identity"
external inj : ('a, 'b) fancy -> ('a, 'b logic) fancy = "%identity"

(* The [token_t] type is use to connect logic variables with environment where they were created *)
type token_env = GT.int
class type virtual ['inh, 'syn] token_env_tt =
  object
    inherit ['inh, 'syn] GT.int_tt
    method t_token_env : 'inh -> token_env -> 'syn
  end
let (token_env :
 (('inh, 'syn) #token_env_tt -> 'inh -> token_env -> 'syn, unit) GT.t) =
  (* The [token_t] type is use to connect logic variables with environment where they were created *)
  let rec token_env_gcata trans inh subj =
    (* The [token_t] type is use to connect logic variables with environment where they were created *)
    GT.int.GT.gcata trans inh subj
  in
  (* The [token_t] type is use to connect logic variables with environment where they were created *)
  {GT.gcata = token_env_gcata;
   (* The [token_t] type is use to connect logic variables with environment where they were created *)
   GT.plugins = ()}
class virtual ['inh, 'syn] token_env_t =
  object (this)
    inherit ['inh, 'syn] GT.int_t
    method t_token_env = GT.transform token_env this
  end
class type show_token_env_env_tt = object inherit GT.show_int_env_tt end
class type gmap_token_env_env_tt = object inherit GT.gmap_int_env_tt end
class type html_token_env_env_tt = object inherit GT.html_int_env_tt end
class type eq_token_env_env_tt = object inherit GT.eq_int_env_tt end
class type compare_token_env_env_tt = object inherit GT.compare_int_env_tt end
class type ['syn] foldl_token_env_env_tt =
  object inherit ['syn] GT.foldl_int_env_tt end
class type ['syn] foldr_token_env_env_tt =
  object inherit ['syn] GT.foldr_int_env_tt end
class show_proto_token_env env =
  object (this)
    inherit [unit, string] token_env_t
    inherit GT.show_proto_int env
  end
class gmap_proto_token_env env =
  object (this)
    inherit [unit, token_env] token_env_t
    inherit GT.gmap_proto_int env
  end
class html_proto_token_env env =
  object (this)
    inherit [unit, HTML.viewer] token_env_t
    inherit GT.html_proto_int env
    method attribute : token_env -> string =
      fun _ ->
        (* The [token_t] type is use to connect logic variables with environment where they were created *)
        ""
    method cname : string -> string =
      fun s ->
        (* The [token_t] type is use to connect logic variables with environment where they were created *)
        s
  end
class eq_proto_token_env env =
  object (this)
    inherit [token_env, bool] token_env_t
    inherit GT.eq_proto_int env
  end
class compare_proto_token_env env =
  object (this)
    inherit [token_env, GT.comparison] token_env_t
    inherit GT.compare_proto_int env
  end
class ['syn] foldl_proto_token_env env =
  object (this)
    inherit ['syn, 'syn] token_env_t
    inherit ['syn] GT.foldl_proto_int env
  end
class ['syn] foldr_proto_token_env env =
  object (this)
    inherit ['syn, 'syn] token_env_t
    inherit ['syn] GT.foldr_proto_int env
  end
class show_token_env_t =
  let self = Obj.magic (ref ()) in
  object (this)
    inherit [unit, string] token_env_t
    inherit show_proto_token_env self
    inherit GT.show_int_t
    inherit GT.show_proto_int self
    initializer
      (* The [token_t] type is use to connect logic variables with environment where they were created *)
      (:=) self (this :> show_token_env_t)
  end
class gmap_token_env_t =
  let self = Obj.magic (ref ()) in
  object (this)
    inherit [unit, token_env] token_env_t
    inherit gmap_proto_token_env self
    inherit GT.gmap_int_t
    inherit GT.gmap_proto_int self
    initializer
      (* The [token_t] type is use to connect logic variables with environment where they were created *)
      (:=) self (this :> gmap_token_env_t)
  end
class html_token_env_t =
  let self = Obj.magic (ref ()) in
  object (this)
    inherit [unit, HTML.viewer] token_env_t
    inherit html_proto_token_env self
    inherit GT.html_int_t
    inherit GT.html_proto_int self
    initializer
      (* The [token_t] type is use to connect logic variables with environment where they were created *)
      (:=) self (this :> html_token_env_t)
    method attribute : token_env -> string =
      fun _ ->
        (* The [token_t] type is use to connect logic variables with environment where they were created *)
        ""
    method cname : string -> string =
      fun s ->
        (* The [token_t] type is use to connect logic variables with environment where they were created *)
        s
  end
class eq_token_env_t =
  let self = Obj.magic (ref ()) in
  object (this)
    inherit [token_env, bool] token_env_t
    inherit eq_proto_token_env self
    inherit GT.eq_int_t
    inherit GT.eq_proto_int self
    initializer
      (* The [token_t] type is use to connect logic variables with environment where they were created *)
      (:=) self (this :> eq_token_env_t)
  end
class compare_token_env_t =
  let self = Obj.magic (ref ()) in
  object (this)
    inherit [token_env, GT.comparison] token_env_t
    inherit compare_proto_token_env self
    inherit GT.compare_int_t
    inherit GT.compare_proto_int self
    initializer
      (* The [token_t] type is use to connect logic variables with environment where they were created *)
      (:=) self (this :> compare_token_env_t)
  end
class ['syn] foldl_token_env_t =
  let self = Obj.magic (ref ()) in
  object (this)
    inherit ['syn, 'syn] token_env_t
    inherit ['syn] foldl_proto_token_env self
    inherit ['syn] GT.foldl_int_t
    inherit ['syn] GT.foldl_proto_int self
    initializer
      (* The [token_t] type is use to connect logic variables with environment where they were created *)
      (:=) self (this :> 'syn foldl_token_env_t)
  end
class ['syn] foldr_token_env_t =
  let self = Obj.magic (ref ()) in
  object (this)
    inherit ['syn, 'syn] token_env_t
    inherit ['syn] foldr_proto_token_env self
    inherit ['syn] GT.foldr_int_t
    inherit ['syn] GT.foldr_proto_int self
    initializer
      (* The [token_t] type is use to connect logic variables with environment where they were created *)
      (:=) self (this :> 'syn foldr_token_env_t)
  end
let (token_env :
 (('inh, 'syn) #token_env_tt -> 'inh -> token_env -> 'syn, < show : token_env -> string; gmap : token_env -> token_env;
 html : token_env -> HTML.viewer; eq : token_env -> token_env -> bool;
 compare : token_env -> token_env -> GT.comparison;
 foldl : 'syn -> token_env -> 'syn; foldr : 'syn -> token_env -> 'syn >)
   GT.t) =
  (* The [token_t] type is use to connect logic variables with environment where they were created *)
  (* The [token_t] type is use to connect logic variables with environment where they were created *)
  {GT.gcata = token_env.GT.gcata;
   (* The [token_t] type is use to connect logic variables with environment where they were created *)
   GT.plugins =
     object
       method show = GT.transform token_env (new show_token_env_t) ()
       method gmap = GT.transform token_env (new gmap_token_env_t) ()
       method html = GT.transform token_env (new html_token_env_t) ()
       method eq = GT.transform token_env (new eq_token_env_t)
       method compare = GT.transform token_env (new compare_token_env_t)
       method foldl = GT.transform token_env (new foldl_token_env_t)
       method foldr = GT.transform token_env (new foldr_token_env_t)
     end}

(* Global token will not be exported outside and will be used to detect the value
 * was actually created by us *)
type token_mk = int list
let global_token : token_mk = [8]

type inner_logic =
    InnerVar of token_mk * token_env * int * Obj.t list

let (!!!) = Obj.magic

let rec bprintf_logic : Buffer.t -> ('a -> unit) -> 'a logic -> unit =
  fun b f x ->
    let rec helper =
      function
        Value x -> f x
      | Var (i, cs) ->
          bprintf b " _.%d" i;
          List.iter (fun x -> bprintf b "=/= "; helper x) cs
    in
    helper x

let rec show_logic f x =
  match x with
    Value x -> f x
  | Var (i, cs) ->
      let c =
        match cs with
          [] -> ""
        | _ ->
            sprintf " %s"
              (GT.show GT.list (fun l -> "=/= " ^ show_logic f l) cs)
      in
      sprintf "_.%d%s" i c


let logic =
  {logic with gcata = ();
   plugins =
     object
       method gmap = logic.plugins#gmap
       method html = logic.plugins#html
       method eq = logic.plugins#eq
       method compare = logic.plugins#compare
       method foldl = logic.plugins#foldl
       method foldr = logic.plugins#foldr
       method show fa x =
         GT.transform logic (GT.lift fa)
           (object
              inherit ['a] show_logic_t
              method c_Var _ s i cs =
                let c =
                  match cs with
                    [] -> ""
                  | _ ->
                      sprintf " %s"
                        (GT.show GT.list (fun l -> "=/= " ^ s.GT.f () l) cs)
                in
                sprintf "_.%d%s" i c
              method c_Value _ _ x = x.GT.fx ()
            end)
           () x
     end}

let (!!) = inj

let inj_pair : ('a, 'c) fancy -> ('b, 'd) fancy -> ('a * 'b, ('c * 'd) logic) fancy =
  fun x y -> x, y

external inj_int : int -> (int, int logic) fancy = "%identity"

exception Not_a_value
exception Occurs_check

module Int =
  struct
    type t = int
    let compare : int -> int -> int = Pervasives.compare
  end
module MultiIntMap :
  sig
    type key = Int.t
    type 'a t
    val empty : 'a t
    val add : key -> 'a -> 'a t -> 'a t
    val find_exn : key -> 'a t -> 'a list
    val replace : key -> 'a list -> 'a t -> 'a t
  end =
  struct
    module M = Map.Make (Int)
    type key = Int.t
    type 'a t = 'a list M.t
    let empty : 'a t = M.empty
    let add k v m =
      try
        let vs = M.find k m in
        let vs = if List.memq v vs then vs else v :: vs in M.add k vs m
      with Not_found -> M.add k [v] m
    let find_exn : key -> 'a t -> 'a list = M.find
    let replace : key -> 'a list -> 'a t -> 'a t = M.add
  end

module Env :
  sig
    type t
    val empty : unit -> t
    val fresh : t -> 'a * t
    val var : t -> 'a -> int option
    val is_var : t -> 'a -> bool
  end =
  struct
    type t =
      { token : token_env;
        mutable next : int;
        mutable reifiers : Obj.t MultiIntMap.t }
    let last_token : token_env ref = ref 0
    let empty () =
      incr last_token;
      {token = !last_token; next = 10; reifiers = MultiIntMap.empty}
    let fresh e =
      let v = InnerVar (global_token, e.token, e.next, []) in
      (!!!) v, {e with next = 1 + e.next}
    let (var_tag, var_size) =
      let dummy_index = 0 in
      let dummy_token = 0 in
      let v = InnerVar (global_token, dummy_token, dummy_index, []) in
      Obj.tag ((!!!) v), Obj.size ((!!!) v)
    let var {token = env_token; _} x =
      (* There we detect if x is a logic variable and then that it belongs to current env *)
      let t = (!!!) x in
      if Obj.tag t = var_tag && Obj.size t = var_size &&
         (let q = Obj.field t 0 in Obj.is_block q && q == (!!!) global_token)
      then
        let q = Obj.field t 1 in
        if Obj.is_int q && q == (!!!) env_token then
          let (InnerVar (_, _, i, _)) = (!!!) x in Some i
        else
          failwith
            "You hacked everything and pass logic variables into wrong environment"
      else None
    let is_var env v = None <> var env v
  end

module Subst :
  sig
    type t
    val empty : t
    type content = { lvar : Obj.t; new_val : Obj.t }
    val make_content : 'a -> 'b -> content
    val of_list : (int * content) list -> t
    val split : t -> Obj.t list * Obj.t list
    val walk : Env.t -> 'a -> t -> 'a
    val unify :
      Env.t -> 'a -> 'a -> t option -> (int * content) list * t option
    val show : t -> string
  end =
  struct
    module M = Map.Make (Int)
    type content = { lvar : Obj.t; new_val : Obj.t }
    type t = content M.t
    let new_val {new_val = x; _} = Obj.obj x
    let lvar {lvar = v; _} = Obj.obj v
    let make_content a b = {lvar = Obj.repr a; new_val = Obj.repr b}
    let show m =
      let b = Buffer.create 40 in
      Buffer.add_string b "subst {";
      M.iter
        (fun i {new_val = new_val} ->
           bprintf b "%d -> %s; " i (generic_show new_val))
        m;
      Buffer.add_string b "}";
      Buffer.contents b
    let empty = M.empty
    let of_list l = List.fold_left (fun s (i, cnt) -> M.add i cnt s) empty l
    let split s =
      M.fold
        (fun _ {lvar = lvar; new_val = new_val} (xs, ts) ->
           lvar :: xs, new_val :: ts)
        s ([], [])
    let rec walk : Env.t -> 'a -> t -> 'a =
      fun env var subst ->
        match Env.var env ((!!!) var) with
          None -> var
        | Some i ->
            try walk env ((@@) new_val (M.find i subst)) subst with
              Not_found -> var
    let rec occurs env xi term subst =
      let y = walk env term subst in
      match Env.var env y with
        Some yi -> xi = yi
      | None ->
          let wy = wrap (Obj.repr y) in
          match wy with
            Invalid n when n = Obj.closure_tag -> false
          | Unboxed _ -> false
          | Invalid n ->
              invalid_arg (sprintf "Invalid value in occurs check (%d)" n)
          | Boxed (_, s, f) ->
              let rec inner i =
                if i >= s then false
                else occurs env xi ((!!!) (f i)) subst || inner (i + 1)
              in
              inner 0
    let unify env x y subst =
      let extend xi x term delta subst =
        if occurs env xi term subst then raise Occurs_check
        else
          let cnt = make_content x term in
          (xi, cnt) :: delta, Some (M.add xi cnt ((!!!) subst))
      in
      let rec unify x y (delta, subst) =
        match subst with
          None -> delta, None
        | Some subst as s ->
            let (x, y) = walk env x subst, walk env y subst in
            match Env.var env x, Env.var env y with
              Some xi, Some yi ->
                if xi = yi then delta, s else extend xi x y delta subst
            | Some xi, _ -> extend xi x y delta subst
            | _, Some yi -> extend yi y x delta subst
            | _ ->
                let (wx, wy) = wrap (Obj.repr x), wrap (Obj.repr y) in
                match wx, wy with
                  Unboxed vx, Unboxed vy ->
                    if vx = vy then delta, s else delta, None
                | Boxed (tx, sx, fx), Boxed (ty, sy, fy) ->
                    if tx = ty && sx = sy then
                      let rec inner i (delta, subst) =
                        match subst with
                          None -> delta, None
                        | Some _ ->
                            if i < sx then
                              inner (i + 1)
                                (unify ((!!!) (fx i)) ((!!!) (fy i))
                                   (delta, subst))
                            else delta, subst
                      in
                      inner 0 (delta, s)
                    else delta, None
                | Invalid n, _ | _, Invalid n ->
                    invalid_arg
                      (sprintf "Invalid values for unification (%d)" n)
                | _ -> delta, None
      in
      unify x y ([], subst)
  end

module State =
  struct
    type t = Env.t * Subst.t * Subst.t list
    let empty () = Env.empty (), Subst.empty, []
    let env (env, _, _) = env
    let show (env, subst, constr) =
      sprintf "st {%s, %s}" (Subst.show subst)
        (GT.show GT.list Subst.show constr)
  end

type goal = State.t -> State.t Stream.t

let call_fresh f (env, subst, constr) =
  let (x, env') = Env.fresh env in f x (env', subst, constr)

exception Disequality_violated

let (===) (x : _ fancy) y (env, subst, constr) =
  (* we should always unify two fancy types *)

  try
    let (prefix, subst') = Subst.unify env x y (Some subst) in
    match subst' with
      None -> Stream.nil
    | Some s ->
        try
          let constr' =
            List.fold_left
              (fun css' cs ->
                 let (x, t) = Subst.split cs in
                 try
                   let (p, s') = Subst.unify env ((!!!) x) ((!!!) t) subst' in
                   match s' with
                     None -> css'
                   | Some _ ->
                       match p with
                         [] -> raise Disequality_violated
                       | _ -> Subst.of_list p :: css'
                 with Occurs_check -> css')
              [] constr
          in
          Stream.cons (env, s, constr') Stream.nil
        with Disequality_violated -> Stream.nil
  with Occurs_check -> Stream.nil

let (=/=) x y (env, subst, constr as st) =
  let normalize_store prefix constr =
    let subst = Subst.of_list prefix in
    let prefix =
      List.split
        (List.map
           Subst.(fun (_, {lvar = lvar; new_val = new_val}) -> lvar, new_val)
           prefix)
    in
    let subsumes subst (vs, ts) =
      try
        match Subst.unify env ((!!!) vs) ((!!!) ts) (Some subst) with
          [], Some _ -> true
        | _ -> false
      with Occurs_check -> false
    in
    let rec traverse =
      function
        [] -> [subst]
      | c :: cs as ccs ->
          if subsumes subst (Subst.split c) then ccs
          else if subsumes c prefix then traverse cs
          else c :: traverse cs
    in
    traverse constr
  in
  try
    let (prefix, subst') = Subst.unify env x y (Some subst) in
    match subst' with
      None -> Stream.cons st Stream.nil
    | Some s ->
        match prefix with
          [] -> Stream.nil
        | _ ->
            let new_constrs = normalize_store prefix constr in
            Stream.cons (env, subst, new_constrs) Stream.nil
  with Occurs_check -> Stream.cons st Stream.nil

let conj f g st = Stream.bind (f st) g

let (&&&) = conj

let disj f g st = Stream.mplus (f st) (g st)

let (|||) = disj

let rec (?|) =
  function
    [h] -> h
  | h :: t -> (|||) h ((?|) t)

let rec (?&) =
  function
    [h] -> h
  | h :: t -> (&&&) h ((?&) t)

let conde = (?|)

module Fresh =
  struct
    let succ prev f = call_fresh (fun x -> prev (f x))
    let zero f = f
    let one f = succ zero f
    let two f = succ one f
    let three f = succ two f
    let four f = succ three f
    let five f = succ four f
    let q = one
    let qr = two
    let qrs = three
    let qrst = four
    let pqrst = five
  end

let success st = Stream.cons st Stream.nil
let failure _ = Stream.nil

exception FreeVarFound
let has_free_vars is_var x =
  let rec walk x =
    if is_var x then raise FreeVarFound
    else
      match wrap (Obj.repr x) with
        Boxed (_tag, size, f) ->
          for i = 0 to size - 1 do walk ((!!!) (f i)) done
      | _ -> ()
  in
  try walk x; false with FreeVarFound -> true

exception WithFreeVars of (Obj.t -> bool) * Obj.t

let rec refine : State.t -> ('a, 'b) fancy -> ('a, 'b) fancy =
  fun (e, s, c as st) x ->
    let rec walk' recursive env var subst =
      let var = Subst.walk env var subst in
      match Env.var env var with
        None ->
          begin match wrap (Obj.repr var) with
            Unboxed _ -> (!!!) var
          | Boxed (t, s, f) ->
              let var = Obj.dup (Obj.repr var) in
              let sf =
                if t = Obj.double_array_tag then (!!!) Obj.set_double_field
                else Obj.set_field
              in
              for i = 0 to s - 1 do
                sf var i ((!!!) (walk' true env ((!!!) (f i)) subst))
              done;
              (!!!) var
          | Invalid n ->
              invalid_arg (sprintf "Invalid value for reconstruction (%d)" n)
          end
      | Some i when recursive ->
          begin match var with
            InnerVar (token1, token2, i, _) ->
              (* We do not add extra Value here: they will be added on manual reification stage *)
              let cs =
                List.fold_left
                  (fun acc s ->
                     match walk' false env ((!!!) var) s with
                       maybeVar when Some i = Env.var env maybeVar -> acc
                     | t -> (!!!) (refine st ((!!!) t)) :: acc)
                  [] c
              in
              (@@) Obj.magic (InnerVar (token1, token2, i, cs))
          end
      | _ -> var
    in
    (!!!) (walk' true e ((!!!) x) s)

module ExtractDeepest =
  struct
    let ext2 x = x
    let succ prev (a, z) = let (foo, base) = prev z in (a, foo), base
  end

module ApplyTuple =
  struct
    let one arg x = x arg
    let succ prev arg (x, y) = x arg, prev arg y
  end

module ApplyLatest =
  struct
    let two = ApplyTuple.one, ExtractDeepest.ext2
    let apply (appf, extf) tup = let (x, base) = extf tup in appf base x
    let succ (appf, extf) = ApplyTuple.succ appf, ExtractDeepest.succ extf
  end

module Uncurry = struct let succ k f (x, y) = k (f x) y end

type var_checker = < isVar : 'a . 'a -> bool >
type ('a, 'b) reification_rez =
    Final of 'a
  | HasFreeVars of var_checker * ('a, 'b) fancy
type ('a, 'b) refiner = State.t Stream.t -> ('a, 'b) reification_rez Stream.t

let refiner : ('a, 'b) fancy -> ('a, 'b) refiner =
  fun x ->
    Stream.map
      (fun (e, _, _ as st) ->
         let ans = refine st ((!!!) x) in
         if has_free_vars (Env.is_var e) (Obj.repr ans) then
           let c : var_checker =
             (!!!) (object method isVar x = Env.is_var e (Obj.repr x) end)
           in
           HasFreeVars (c, (!!!) ans)
         else Final ((!!!) ans))

module LogicAdder =
  struct
    let zero f = f
    let succ prev f =
      call_fresh (fun logic st -> refiner logic, prev (f logic) st)
  end

let one () = (fun x -> LogicAdder.(succ zero) x), (@@), ApplyLatest.two

let succ n () =
  let (adder, currier, app) = n () in
  LogicAdder.succ adder, Uncurry.succ currier, ApplyLatest.succ app

let two () = succ one ()
let three () = succ two ()
let four () = succ three ()
let five () = succ four ()

let q = one
let qr = two
let qrs = three
let qrst = four
let pqrst = five

let run n goalish f =
  let (adder, currier, app_num) = n () in
  let run f = f (State.empty ()) in
  (|>) ((|>) (run (adder goalish)) (ApplyLatest.apply app_num)) (currier f)

(* ************************************************************************** *)
module type T1 = sig type 'a t val fmap : ('a -> 'b) -> 'a t -> 'b t end
module type T2 =
  sig
    type ('a, 'b) t
    val fmap : ('a -> 'c) -> ('b -> 'd) -> ('a, 'b) t -> ('c, 'd) t
  end
module type T3 =
  sig
    type ('a, 'b, 'c) t
    val fmap :
      ('a -> 'q) -> ('b -> 'r) -> ('c -> 's) -> ('a, 'b, 'c) t ->
        ('q, 'r, 's) t
  end

let var_of_fancy_exn : var_checker -> ('a, 'b) fancy -> (var_checker -> ('a, 'b) fancy -> 'b) -> 'b =
  fun c x r ->
    if c#isVar x then
      let (InnerVar (_, _, n, cstr)) = (!!!) x in
      (!!!) (Var (n, List.map ((!!!) (r c)) ((!!!) cstr)))
    else failwith "Bad argument of var_of_fancy: it should be logic variable"

module Fmap1 (T : T1) =
  struct
    external distrib :
      ('a, 'b) fancy T.t -> ('a T.t, 'b T.t) fancy = "%identity"
    let rec reifier : (var_checker -> ('a, 'b) fancy -> 'b) -> var_checker ->
      ('a T.t, 'b T.t logic as 'r) fancy -> 'r =
      fun arg_r c x ->
        if c#isVar x then var_of_fancy_exn c x (reifier arg_r)
        else Value (T.fmap (arg_r c) x)
  end

module Fmap2 (T : T2) =
  struct
    external distrib :
      (('a, 'b) fancy, ('c, 'd) fancy) T.t ->
        (('a, 'b) T.t, ('c, 'd) T.t) fancy = "%identity"
    let rec reifier : (var_checker -> ('a, 'b) fancy -> 'b) ->
      (var_checker -> ('c, 'd) fancy -> 'd) -> var_checker ->
      (('a, 'c) T.t, ('b, 'd) T.t logic) fancy -> ('b, 'd) T.t logic =
      fun r1 r2 c x ->
        if c#isVar x then var_of_fancy_exn c x (reifier r1 r2)
        else Value (T.fmap (r1 c) (r2 c) x)
  end

module Fmap3 (T : T3) =
  struct
    type ('a, 'b, 'c) t = ('a, 'b, 'c) T.t
    external distrib :
      (('a, 'b) fancy, ('c, 'd) fancy, ('e, 'f) fancy) t ->
        (('a, 'c, 'e) t, ('b, 'd, 'f) t) fancy = "%identity"
    let rec reifier r1 r2 r3 (c : var_checker) x =
      if c#isVar x then var_of_fancy_exn c x (reifier r1 r2 r3)
      else Value (T.fmap (r1 c) (r2 c) (r3 c) x)
  end

module Pair =
  struct
    module X =
      struct
        type ('a, 'b) t = 'a * 'b
        let fmap f g (x, y) = f x, g y
      end
    include Fmap2 (X)
  end

module ManualReifiers =
  struct
    let rec simple_reifier : var_checker -> ('a, 'a logic) fancy -> 'a logic =
      fun c n ->
        if c#isVar n then var_of_fancy_exn c n simple_reifier else Value n
    let bool_reifier : var_checker -> (bool, bool logic) fancy -> bool logic =
      simple_reifier
    let rec int_reifier : var_checker -> (int, int logic) fancy -> int logic =
      fun c n ->
        if c#isVar n then var_of_fancy_exn c n int_reifier else Value n
    let rec string_reifier : var_checker -> (string, string logic) fancy -> string logic =
      fun c x ->
        if c#isVar x then var_of_fancy_exn c x string_reifier else Value x
    let rec pair_reifier : (var_checker -> ('a, 'b) fancy -> 'b) ->
      (var_checker -> ('c, 'd) fancy -> 'd) -> var_checker ->
      ('a * 'c, ('b * 'd) logic as 'r) fancy -> 'r =
      fun r1 r2 c p ->
        if c#isVar p then var_of_fancy_exn c p (pair_reifier r1 r2)
        else Pair.reifier r1 r2 c p
  end

(* ***************************** a la relational StdLib here ***************  *)
type ('a, 'l) llist =
    Nil
  | Cons of 'a * 'l  [@@deriving show { with_path = false }]
class type virtual ['a, 'ia, 'sa, 'l, 'il, 'sl, 'inh, 'syn] llist_tt =
  object
    method c_Nil :
      'inh ->
        ('inh, ('a, 'l) llist, 'syn, < a : 'ia -> 'a -> 'sa; l : 'il -> 'l -> 'sl >)
          GT.a ->
        'syn
    method c_Cons :
      'inh ->
        ('inh, ('a, 'l) llist, 'syn, < a : 'ia -> 'a -> 'sa; l : 'il -> 'l -> 'sl >)
          GT.a ->
        ('ia, 'a, 'sa, < a : 'ia -> 'a -> 'sa; l : 'il -> 'l -> 'sl >) GT.a ->
        ('il, 'l, 'sl, < a : 'ia -> 'a -> 'sa; l : 'il -> 'l -> 'sl >) GT.a ->
        'syn
    method t_llist :
      ('ia -> 'a -> 'sa) -> ('il -> 'l -> 'sl) -> 'inh -> ('a, 'l) llist ->
        'syn
  end
let (llist :
 (('ia -> 'a -> 'sa) -> ('il -> 'l -> 'sl) ->
   ('a, 'ia, 'sa, 'l, 'il, 'sl, 'inh, 'syn) #llist_tt -> 'inh ->
   ('a, 'l) llist -> 'syn, unit)
   GT.t) =
  (* ***************************** a la relational StdLib here ***************  *)
  let rec llist_gcata fa fl trans inh subj =
    (* ***************************** a la relational StdLib here ***************  *)
    let rec self =
      (* ***************************** a la relational StdLib here ***************  *)
      llist_gcata fa fl trans
    and tpo =
      (* ***************************** a la relational StdLib here ***************  *)
      object method a = fa method l = fl end
    in
    match subj with
      Nil ->
        (* ***************************** a la relational StdLib here ***************  *)
        trans#c_Nil inh (GT.make self subj tpo)
    | Cons (p0, p1) ->
        (* ***************************** a la relational StdLib here ***************  *)
        trans#c_Cons inh (GT.make self subj tpo) (GT.make fa p0 tpo)
          (GT.make fl p1 tpo)
  in
  (* ***************************** a la relational StdLib here ***************  *)
  {GT.gcata = llist_gcata;
   (* ***************************** a la relational StdLib here ***************  *)
   GT.plugins = ()}
class virtual ['a, 'ia, 'sa, 'l, 'il, 'sl, 'inh, 'syn] llist_t =
  object (this)
    method virtual c_Nil :
      'inh ->
        ('inh, ('a, 'l) llist, 'syn, < a : 'ia -> 'a -> 'sa; l : 'il -> 'l -> 'sl >)
          GT.a ->
        'syn
    method virtual c_Cons :
      'inh ->
        ('inh, ('a, 'l) llist, 'syn, < a : 'ia -> 'a -> 'sa; l : 'il -> 'l -> 'sl >)
          GT.a ->
        ('ia, 'a, 'sa, < a : 'ia -> 'a -> 'sa; l : 'il -> 'l -> 'sl >) GT.a ->
        ('il, 'l, 'sl, < a : 'ia -> 'a -> 'sa; l : 'il -> 'l -> 'sl >) GT.a ->
        'syn
    method t_llist fa fl = GT.transform llist fa fl this
  end
class type ['a, 'l] show_llist_env_tt = object  end
class type ['a, 'sa, 'l, 'sl] gmap_llist_env_tt = object  end
class type ['a, 'l] html_llist_env_tt = object  end
class type ['a, 'l] eq_llist_env_tt = object  end
class type ['a, 'l] compare_llist_env_tt = object  end
class type ['a, 'l, 'syn] foldl_llist_env_tt = object  end
class type ['a, 'l, 'syn] foldr_llist_env_tt = object  end
class ['a, 'l] show_proto_llist env =
  object (this)
    inherit ['a, unit, string, 'l, unit, string, unit, string] llist_t
    method c_Cons inh subj p0 p1 =
      (("Cons (" ^ p0.GT.fx ()) ^ ", " ^ p1.GT.fx ()) ^ ")"
    method c_Nil inh subj = "Nil (" ^ ")"
  end
class ['a, 'sa, 'l, 'sl] gmap_proto_llist env =
  object (this)
    inherit ['a, unit, 'sa, 'l, unit, 'sl, unit, ('sa, 'sl) llist] llist_t
    method c_Cons inh subj p0 p1 = Cons (p0.GT.fx (), p1.GT.fx ())
    method c_Nil inh subj = Nil
  end
class ['a, 'l] html_proto_llist env =
  object (this)
    inherit
      ['a, unit, HTML.viewer, 'l, unit, HTML.viewer, unit, HTML.viewer]
        llist_t
    method c_Cons inh subj p0 p1 =
      View.concat (HTML.b (HTML.string (this#cname "Cons")))
        (HTML.ul
           (View.concat (View.concat View.empty (p0.GT.fx ())) (p1.GT.fx ())))
    method c_Nil inh subj =
      View.concat (HTML.b (HTML.string (this#cname "Nil")))
        (HTML.ul View.empty)
    method attribute : ('a, 'l) llist -> string =
      fun _ ->
        (* ***************************** a la relational StdLib here ***************  *)
        ""
    method cname : string -> string =
      fun s ->
        (* ***************************** a la relational StdLib here ***************  *)
        s
  end
class ['a, 'l] eq_proto_llist env =
  object (this)
    inherit ['a, 'a, bool, 'l, 'l, bool, ('a, 'l) llist, bool] llist_t
    method c_Cons inh subj p0 p1 =
      match inh with
        Cons (p0_, p1_) ->
          (* ***************************** a la relational StdLib here ***************  *)
          (true && p0.GT.fx p0_) && p1.GT.fx p1_
      | _ ->
          (* ***************************** a la relational StdLib here ***************  *)
          false
    method c_Nil inh subj =
      match inh with
        Nil ->
          (* ***************************** a la relational StdLib here ***************  *)
          true
      | _ ->
          (* ***************************** a la relational StdLib here ***************  *)
          false
  end
class ['a, 'l] compare_proto_llist env =
  object (this)
    inherit
      ['a, 'a, GT.comparison, 'l, 'l, GT.comparison, ('a, 'l) llist,
      GT.comparison]
        llist_t
    method c_Cons inh subj p0 p1 =
      match inh with
        Cons (p0_, p1_) ->
          (* ***************************** a la relational StdLib here ***************  *)
          GT.chain_compare
            (GT.chain_compare GT.EQ
               (fun _ ->
                  (* ***************************** a la relational StdLib here ***************  *)
                  p0.GT.fx p0_))
            (fun _ ->
               (* ***************************** a la relational StdLib here ***************  *)
               p1.GT.fx p1_)
      | other ->
          (* ***************************** a la relational StdLib here ***************  *)
          GT.compare_vari other subj.GT.x
    method c_Nil inh subj =
      match inh with
        Nil ->
          (* ***************************** a la relational StdLib here ***************  *)
          GT.EQ
      | other ->
          (* ***************************** a la relational StdLib here ***************  *)
          GT.compare_vari other subj.GT.x
  end
class ['a, 'l, 'syn] foldl_proto_llist env =
  object (this)
    inherit ['a, 'syn, 'syn, 'l, 'syn, 'syn, 'syn, 'syn] llist_t
    method c_Cons inh subj p0 p1 = p1.GT.fx (p0.GT.fx inh)
    method c_Nil inh subj = inh
  end
class ['a, 'l, 'syn] foldr_proto_llist env =
  object (this)
    inherit ['a, 'syn, 'syn, 'l, 'syn, 'syn, 'syn, 'syn] llist_t
    method c_Cons inh subj p0 p1 = p0.GT.fx (p1.GT.fx inh)
    method c_Nil inh subj = inh
  end
class ['a, 'l] show_llist_t =
  let self = Obj.magic (ref ()) in
  object (this)
    inherit ['a, unit, string, 'l, unit, string, unit, string] llist_t
    inherit ['a, 'l] show_proto_llist self
    initializer
      (* ***************************** a la relational StdLib here ***************  *)
      (:=) self (this :> ('a, 'l) show_llist_t)
  end
class ['a, 'sa, 'l, 'sl] gmap_llist_t =
  let self = Obj.magic (ref ()) in
  object (this)
    inherit ['a, unit, 'sa, 'l, unit, 'sl, unit, ('sa, 'sl) llist] llist_t
    inherit ['a, 'sa, 'l, 'sl] gmap_proto_llist self
    initializer
      (* ***************************** a la relational StdLib here ***************  *)
      (:=) self (this :> ('a, 'sa, 'l, 'sl) gmap_llist_t)
  end
class ['a, 'l] html_llist_t =
  let self = Obj.magic (ref ()) in
  object (this)
    inherit
      ['a, unit, HTML.viewer, 'l, unit, HTML.viewer, unit, HTML.viewer]
        llist_t
    inherit ['a, 'l] html_proto_llist self
    initializer
      (* ***************************** a la relational StdLib here ***************  *)
      (:=) self (this :> ('a, 'l) html_llist_t)
    method attribute : ('a, 'l) llist -> string =
      fun _ ->
        (* ***************************** a la relational StdLib here ***************  *)
        ""
    method cname : string -> string =
      fun s ->
        (* ***************************** a la relational StdLib here ***************  *)
        s
  end
class ['a, 'l] eq_llist_t =
  let self = Obj.magic (ref ()) in
  object (this)
    inherit ['a, 'a, bool, 'l, 'l, bool, ('a, 'l) llist, bool] llist_t
    inherit ['a, 'l] eq_proto_llist self
    initializer
      (* ***************************** a la relational StdLib here ***************  *)
      (:=) self (this :> ('a, 'l) eq_llist_t)
  end
class ['a, 'l] compare_llist_t =
  let self = Obj.magic (ref ()) in
  object (this)
    inherit
      ['a, 'a, GT.comparison, 'l, 'l, GT.comparison, ('a, 'l) llist,
      GT.comparison]
        llist_t
    inherit ['a, 'l] compare_proto_llist self
    initializer
      (* ***************************** a la relational StdLib here ***************  *)
      (:=) self (this :> ('a, 'l) compare_llist_t)
  end
class ['a, 'l, 'syn] foldl_llist_t =
  let self = Obj.magic (ref ()) in
  object (this)
    inherit ['a, 'syn, 'syn, 'l, 'syn, 'syn, 'syn, 'syn] llist_t
    inherit ['a, 'l, 'syn] foldl_proto_llist self
    initializer
      (* ***************************** a la relational StdLib here ***************  *)
      (:=) self (this :> ('a, 'l, 'syn) foldl_llist_t)
  end
class ['a, 'l, 'syn] foldr_llist_t =
  let self = Obj.magic (ref ()) in
  object (this)
    inherit ['a, 'syn, 'syn, 'l, 'syn, 'syn, 'syn, 'syn] llist_t
    inherit ['a, 'l, 'syn] foldr_proto_llist self
    initializer
      (* ***************************** a la relational StdLib here ***************  *)
      (:=) self (this :> ('a, 'l, 'syn) foldr_llist_t)
  end
let (llist :
 (('ia -> 'a -> 'sa) -> ('il -> 'l -> 'sl) ->
   ('a, 'ia, 'sa, 'l, 'il, 'sl, 'inh, 'syn) #llist_tt -> 'inh ->
   ('a, 'l) llist -> 'syn, < show : ('a -> string) -> ('l -> string) -> ('a, 'l) llist -> string;
 gmap : ('a -> 'sa) -> ('l -> 'sl) -> ('a, 'l) llist -> ('sa, 'sl) llist;
 html :
   ('a -> HTML.viewer) -> ('l -> HTML.viewer) -> ('a, 'l) llist ->
     HTML.viewer;
 eq :
   ('a -> 'a -> bool) -> ('l -> 'l -> bool) -> ('a, 'l) llist ->
     ('a, 'l) llist -> bool;
 compare :
   ('a -> 'a -> GT.comparison) -> ('l -> 'l -> GT.comparison) ->
     ('a, 'l) llist -> ('a, 'l) llist -> GT.comparison;
 foldl :
   ('syn -> 'a -> 'syn) -> ('syn -> 'l -> 'syn) -> 'syn -> ('a, 'l) llist ->
     'syn;
 foldr :
   ('syn -> 'a -> 'syn) -> ('syn -> 'l -> 'syn) -> 'syn -> ('a, 'l) llist ->
     'syn >)
   GT.t) =
  (* ***************************** a la relational StdLib here ***************  *)
  (* ***************************** a la relational StdLib here ***************  *)
  {GT.gcata = llist.GT.gcata;
   (* ***************************** a la relational StdLib here ***************  *)
   GT.plugins =
     object
       method show a l =
         GT.transform llist (GT.lift a) (GT.lift l) (new show_llist_t) ()
       method gmap a l =
         GT.transform llist (GT.lift a) (GT.lift l) (new gmap_llist_t) ()
       method html a l =
         GT.transform llist (GT.lift a) (GT.lift l) (new html_llist_t) ()
       method eq a l = GT.transform llist a l (new eq_llist_t)
       method compare a l = GT.transform llist a l (new compare_llist_t)
       method foldl a l = GT.transform llist a l (new foldl_llist_t)
       method foldr a l = GT.transform llist a l (new foldr_llist_t)
     end}
type 'a lnat =
    O
  | S of 'a
class type virtual ['a, 'ia, 'sa, 'inh, 'syn] lnat_tt =
  object
    method c_O :
      'inh -> ('inh, 'a lnat, 'syn, < a : 'ia -> 'a -> 'sa >) GT.a -> 'syn
    method c_S :
      'inh -> ('inh, 'a lnat, 'syn, < a : 'ia -> 'a -> 'sa >) GT.a ->
        ('ia, 'a, 'sa, < a : 'ia -> 'a -> 'sa >) GT.a -> 'syn
    method t_lnat : ('ia -> 'a -> 'sa) -> 'inh -> 'a lnat -> 'syn
  end
let (lnat :
 (('ia -> 'a -> 'sa) -> ('a, 'ia, 'sa, 'inh, 'syn) #lnat_tt -> 'inh ->
   'a lnat -> 'syn, unit)
   GT.t) =
  let rec lnat_gcata fa trans inh subj =
    let rec self = lnat_gcata fa trans
    and tpo = object method a = fa end in
    match subj with
      O -> trans#c_O inh (GT.make self subj tpo)
    | S p0 -> trans#c_S inh (GT.make self subj tpo) (GT.make fa p0 tpo)
  in
  {GT.gcata = lnat_gcata; GT.plugins = ()}
class virtual ['a, 'ia, 'sa, 'inh, 'syn] lnat_t =
  object (this)
    method virtual c_O :
      'inh -> ('inh, 'a lnat, 'syn, < a : 'ia -> 'a -> 'sa >) GT.a -> 'syn
    method virtual c_S :
      'inh -> ('inh, 'a lnat, 'syn, < a : 'ia -> 'a -> 'sa >) GT.a ->
        ('ia, 'a, 'sa, < a : 'ia -> 'a -> 'sa >) GT.a -> 'syn
    method t_lnat fa = GT.transform lnat fa this
  end
class type ['a] show_lnat_env_tt = object  end
class type ['a] html_lnat_env_tt = object  end
class type ['a] eq_lnat_env_tt = object  end
class type ['a] compare_lnat_env_tt = object  end
class type ['a, 'syn] foldl_lnat_env_tt = object  end
class type ['a, 'syn] foldr_lnat_env_tt = object  end
class type ['a, 'sa] gmap_lnat_env_tt = object  end
class ['a] show_proto_lnat env =
  object (this)
    inherit ['a, unit, string, unit, string] lnat_t
    method c_S inh subj p0 = ("S (" ^ p0.GT.fx ()) ^ ")"
    method c_O inh subj = "O (" ^ ")"
  end
class ['a] html_proto_lnat env =
  object (this)
    inherit ['a, unit, HTML.viewer, unit, HTML.viewer] lnat_t
    method c_S inh subj p0 =
      View.concat (HTML.b (HTML.string (this#cname "S")))
        (HTML.ul (View.concat View.empty (p0.GT.fx ())))
    method c_O inh subj =
      View.concat (HTML.b (HTML.string (this#cname "O"))) (HTML.ul View.empty)
    method attribute : 'a lnat -> string = fun _ -> ""
    method cname : string -> string = fun s -> s
  end
class ['a] eq_proto_lnat env =
  object (this)
    inherit ['a, 'a, bool, 'a lnat, bool] lnat_t
    method c_S inh subj p0 =
      match inh with
        S p0_ -> true && p0.GT.fx p0_
      | _ -> false
    method c_O inh subj =
      match inh with
        O -> true
      | _ -> false
  end
class ['a] compare_proto_lnat env =
  object (this)
    inherit ['a, 'a, GT.comparison, 'a lnat, GT.comparison] lnat_t
    method c_S inh subj p0 =
      match inh with
        S p0_ -> GT.chain_compare GT.EQ (fun _ -> p0.GT.fx p0_)
      | other -> GT.compare_vari other subj.GT.x
    method c_O inh subj =
      match inh with
        O -> GT.EQ
      | other -> GT.compare_vari other subj.GT.x
  end
class ['a, 'syn] foldl_proto_lnat env =
  object (this)
    inherit ['a, 'syn, 'syn, 'syn, 'syn] lnat_t
    method c_S inh subj p0 = p0.GT.fx inh
    method c_O inh subj = inh
  end
class ['a, 'syn] foldr_proto_lnat env =
  object (this)
    inherit ['a, 'syn, 'syn, 'syn, 'syn] lnat_t
    method c_S inh subj p0 = p0.GT.fx inh
    method c_O inh subj = inh
  end
class ['a, 'sa] gmap_proto_lnat env =
  object (this)
    inherit ['a, unit, 'sa, unit, 'sa lnat] lnat_t
    method c_S inh subj p0 = S (p0.GT.fx ())
    method c_O inh subj = O
  end
class ['a] show_lnat_t =
  let self = Obj.magic (ref ()) in
  object (this)
    inherit ['a, unit, string, unit, string] lnat_t
    inherit ['a] show_proto_lnat self
    initializer (:=) self (this :> 'a show_lnat_t)
  end
class ['a] html_lnat_t =
  let self = Obj.magic (ref ()) in
  object (this)
    inherit ['a, unit, HTML.viewer, unit, HTML.viewer] lnat_t
    inherit ['a] html_proto_lnat self
    initializer (:=) self (this :> 'a html_lnat_t)
    method attribute : 'a lnat -> string = fun _ -> ""
    method cname : string -> string = fun s -> s
  end
class ['a] eq_lnat_t =
  let self = Obj.magic (ref ()) in
  object (this)
    inherit ['a, 'a, bool, 'a lnat, bool] lnat_t
    inherit ['a] eq_proto_lnat self
    initializer (:=) self (this :> 'a eq_lnat_t)
  end
class ['a] compare_lnat_t =
  let self = Obj.magic (ref ()) in
  object (this)
    inherit ['a, 'a, GT.comparison, 'a lnat, GT.comparison] lnat_t
    inherit ['a] compare_proto_lnat self
    initializer (:=) self (this :> 'a compare_lnat_t)
  end
class ['a, 'syn] foldl_lnat_t =
  let self = Obj.magic (ref ()) in
  object (this)
    inherit ['a, 'syn, 'syn, 'syn, 'syn] lnat_t
    inherit ['a, 'syn] foldl_proto_lnat self
    initializer (:=) self (this :> ('a, 'syn) foldl_lnat_t)
  end
class ['a, 'syn] foldr_lnat_t =
  let self = Obj.magic (ref ()) in
  object (this)
    inherit ['a, 'syn, 'syn, 'syn, 'syn] lnat_t
    inherit ['a, 'syn] foldr_proto_lnat self
    initializer (:=) self (this :> ('a, 'syn) foldr_lnat_t)
  end
class ['a, 'sa] gmap_lnat_t =
  let self = Obj.magic (ref ()) in
  object (this)
    inherit ['a, unit, 'sa, unit, 'sa lnat] lnat_t
    inherit ['a, 'sa] gmap_proto_lnat self
    initializer (:=) self (this :> ('a, 'sa) gmap_lnat_t)
  end
let (lnat :
 (('ia -> 'a -> 'sa) -> ('a, 'ia, 'sa, 'inh, 'syn) #lnat_tt -> 'inh ->
   'a lnat -> 'syn, < show : ('a -> string) -> 'a lnat -> string;
 html : ('a -> HTML.viewer) -> 'a lnat -> HTML.viewer;
 eq : ('a -> 'a -> bool) -> 'a lnat -> 'a lnat -> bool;
 compare : ('a -> 'a -> GT.comparison) -> 'a lnat -> 'a lnat -> GT.comparison;
 foldl : ('syn -> 'a -> 'syn) -> 'syn -> 'a lnat -> 'syn;
 foldr : ('syn -> 'a -> 'syn) -> 'syn -> 'a lnat -> 'syn;
 gmap : ('a -> 'sa) -> 'a lnat -> 'sa lnat >)
   GT.t) =
  {GT.gcata = lnat.GT.gcata;
   GT.plugins =
     object
       method show a = GT.transform lnat (GT.lift a) (new show_lnat_t) ()
       method html a = GT.transform lnat (GT.lift a) (new html_lnat_t) ()
       method eq a = GT.transform lnat a (new eq_lnat_t)
       method compare a = GT.transform lnat a (new compare_lnat_t)
       method foldl a = GT.transform lnat a (new foldl_lnat_t)
       method foldr a = GT.transform lnat a (new foldr_lnat_t)
       method gmap a = GT.transform lnat (GT.lift a) (new gmap_lnat_t) ()
     end}

let none () = (@@) inj (lift None)
let some x = (@@) inj (lift (Some x))

module Bool =
  struct
    type 'a logic' = 'a logic
    let logic' = logic
    type ground = bool
    let ground =
      {GT.gcata = ();
       GT.plugins =
         object (this)
           method html n = GT.html GT.bool n
           method eq n m = GT.eq GT.bool n m
           method compare n m = GT.compare GT.bool n m
           method foldr n = GT.foldr GT.bool n
           method foldl n = GT.foldl GT.bool n
           method gmap n = GT.gmap GT.bool n
           method show n = GT.show GT.bool n
         end}
    type logic = bool logic'
    let logic =
      {GT.gcata = ();
       GT.plugins =
         object (this)
           method html n = GT.html logic' (GT.html ground) n
           method eq n m = GT.eq logic' (GT.eq ground) n m
           method compare n m = GT.compare logic' (GT.compare ground) n m
           method foldr a n = GT.foldr logic' (GT.foldr ground) a n
           method foldl a n = GT.foldl logic' (GT.foldl ground) a n
           method gmap n = GT.gmap logic' (GT.gmap ground) n
           method show n = GT.show logic' (GT.show ground) n
         end}
    type boolf = (bool, bool logic') fancy
    type groundf = boolf
    type fancy = groundf
    let false_ : boolf = (@@) inj (lift false)
    let true_ : boolf = (@@) inj (lift true)
    let (|^) a b c =
      conde
        [(&&&) ((&&&) ((===) a false_) ((===) b false_)) ((===) c true_);
         (&&&) ((&&&) ((===) a false_) ((===) b true_)) ((===) c true_);
         (&&&) ((&&&) ((===) a true_) ((===) b false_)) ((===) c true_);
         (&&&) ((&&&) ((===) a true_) ((===) b true_)) ((===) c false_)]
    let noto' a na = (|^) a a na
    let noto a = noto' a true_
    let oro a b c =
      Fresh.two
        (fun aa bb ->
           (&&&) ((&&&) ((|^) a a aa) ((|^) b b bb)) ((|^) aa bb c))
    let ando a b c = Fresh.one (fun ab -> (&&&) ((|^) a b ab) ((|^) ab ab c))
    let (&&) a b = ando a b true_
    let (||) a b = oro a b true_
    let show_ground : ground -> string = string_of_bool
    let inj b : boolf = (@@) inj (lift b)
  end

let eqo x y t =
  conde
    [(&&&) ((===) x y) ((===) t Bool.true_);
     (&&&) ((=/=) x y) ((===) t Bool.false_)]

let neqo x y t =
  conde
    [(&&&) ((=/=) x y) ((===) t Bool.true_);
     (&&&) ((===) x y) ((===) t Bool.false_)]

module Nat =
  struct
    type 'a logic' = 'a logic
    let logic' = logic
    module X =
      struct
        type 'a t = 'a lnat
        let fmap f =
          function
            O -> O
          | S n -> S (f n)
      end
    include X
    module F = Fmap1 (X)
    type ground = ground t
    type logic = logic t logic'
    type groundf = (ground, logic) fancy
    let rec reifier : var_checker -> (ground, logic) fancy -> logic =
      fun c x ->
        if c#isVar x then var_of_fancy_exn c x reifier
        else F.reifier reifier c x
    let ground =
      {GT.gcata = ();
       GT.plugins =
         object (this)
           method html n = GT.html lnat this#html n
           method eq n = GT.eq lnat this#eq n
           method compare n = GT.compare lnat this#compare n
           method foldr n = GT.foldr lnat this#foldr n
           method foldl n = GT.foldl lnat this#foldl n
           method gmap n = GT.gmap lnat this#gmap n
           method show n = GT.show lnat this#show n
         end}
    let logic =
      {GT.gcata = ();
       GT.plugins =
         object (this)
           method html n = GT.html logic' (GT.html lnat this#html) n
           method eq n m = GT.eq logic' (GT.eq lnat this#eq) n m
           method compare n m =
             GT.compare logic' (GT.compare lnat this#compare) n m
           method foldr a n = GT.foldr logic' (GT.foldr lnat this#foldr) a n
           method foldl a n = GT.foldl logic' (GT.foldl lnat this#foldl) a n
           method gmap n = GT.gmap logic' (GT.gmap lnat this#gmap) n
           method show n = GT.show logic' (GT.show lnat this#show) n
         end}
    let rec of_int n = if n <= 0 then O else S (of_int (n - 1))
    let rec to_int =
      function
        O -> 0
      | S n -> 1 + to_int n
    let o = (@@) inj (lift O)
    let s x = (@@) inj (lift (S x))
    let rec addo x y z =
      conde
        [(&&&) ((===) x o) ((===) z y);
         Fresh.two
           (fun x' z' ->
              (&&&) ((&&&) ((===) x (s x')) ((===) z (s z'))) (addo x' y z'))]
    let (+) = addo
    let rec mulo x y z =
      conde
        [(&&&) ((===) x o) ((===) z o);
         Fresh.two
           (fun x' z' ->
              (&&&) ((===) x (s x')) ((&&&) (addo y z' z) (mulo x' y z')))]
    let ( * ) = mulo
    let rec leo x y b =
      conde
        [(&&&) ((===) x o) ((===) b Bool.true_);
         (&&&) ((&&&) ((=/=) x o) ((===) y o)) ((===) b Bool.false_);
         Fresh.two
           (fun x' y' ->
              (&&&) ((&&&) ((===) x (s x')) ((===) y (s y'))) (leo x' y' b))]
    let geo x y b = leo y x b
    let (<=) x y = leo x y Bool.true_
    let (>=) x y = geo x y Bool.false_
    let rec gto x y b =
      conde
        [(&&&) ((&&&) ((=/=) x o) ((===) y o)) ((===) b Bool.true_);
         (&&&) ((===) x o) ((===) b Bool.false_);
         Fresh.two
           (fun x' y' ->
              (&&&) ((&&&) ((===) x (s x')) ((===) y (s y'))) (gto x' y' b))]
    let lto x y b = gto y x b
    let (>) x y = gto x y Bool.true_
    let (<) x y = lto x y Bool.true_
    let show_ground : ground -> string = GT.show ground
  end

let rec inj_nat n = if n <= 0 then inj O else inj (S ((@@) inj_nat (n - 1)))

module List =
  struct
    include List
    type 'a logic' = 'a logic
    let pp_logic' = pp_logic
    let logic' = logic
    type ('a, 'l) t = ('a, 'l) llist  [@@deriving show { with_path = false }]
    module X =
      struct
        type ('a, 'b) t = ('a, 'b) llist
        let fmap f g =
          function
            Nil -> Nil
          | Cons (x, xs) -> Cons (f x, g xs)
      end
    module F = Fmap2 (X)
    let nil () = inj (F.distrib Nil)
    let cons x y = inj (F.distrib (Cons (x, y)))
    type 'a ground = ('a, 'a ground) t [@@deriving show { with_path = false }]


    type 'a logic = ('a, 'a logic) t logic' [@@deriving show { with_path = false }]
    let rec reifier : _ -> var_checker -> (_ ground, 'b logic) fancy -> 'b logic =
      fun arg_r c x ->
        if c#isVar x then var_of_fancy_exn c x (reifier arg_r)
        else F.reifier arg_r (reifier arg_r) c x
    let rec of_list =
      function
        [] -> nil ()
      | x :: xs -> cons x (of_list xs)
    let ground =
      {GT.gcata = ();
       GT.plugins =
         object (this)
           method html fa l = GT.html llist fa (this#html fa) l
           method eq fa l = GT.eq llist fa (this#eq fa) l
           method compare fa l = GT.compare llist fa (this#compare fa) l
           method foldr fa l = GT.foldr llist fa (this#foldr fa) l
           method foldl fa l = GT.foldl llist fa (this#foldl fa) l
           method gmap fa l = GT.gmap llist fa (this#gmap fa) l
           method show fa l =
             "[" ^
             (let rec inner l =
                GT.transform llist (GT.lift fa) (GT.lift inner)
                  (object
                     inherit ['a, 'a ground] show_llist_t
                     method c_Nil _ _ = ""
                     method c_Cons i s x xs =
                       x.GT.fx () ^
                       (match xs.GT.x with
                          Nil -> ""
                        | _ -> "; " ^ xs.GT.fx ())
                   end)
                  () l
              in
              inner l ^ "]")
         end}
    let logic =
      {GT.gcata = ();
       GT.plugins =
         object (this)
           method compare fa l =
             GT.compare logic' (GT.compare llist fa (this#compare fa)) l
           method gmap fa l =
             GT.gmap logic' (GT.gmap llist fa (this#gmap fa)) l
           method eq fa l = GT.eq logic' (GT.eq llist fa (this#eq fa)) l
           method foldl fa l =
             GT.foldl logic' (GT.foldl llist fa (this#foldl fa)) l
           method foldr fa l =
             GT.foldr logic' (GT.foldr llist fa (this#foldr fa)) l
           method html fa l =
             GT.html logic' (GT.html llist fa (this#html fa)) l
           method show : ('a -> string) -> 'a logic -> GT.string =
             fun fa l ->
               GT.show logic'
                 (fun l ->
                    "[" ^
                    (let rec inner l =
                       GT.transform llist (GT.lift fa)
                         (GT.lift (GT.show logic inner))
                         (object
                            inherit ['a, 'a logic] show_llist_t
                            method c_Nil _ _ = ""
                            method c_Cons i s x xs =
                              x.GT.fx () ^
                              (match xs.GT.x with
                                 Value Nil -> ""
                               | _ -> "; " ^ xs.GT.fx ())
                          end)
                         () l
                     in
                     inner l ^ "]"))
                 l
         end}
    type ('a, 'b) flist = ('a ground, 'b logic) fancy
    let flist =
      {GT.gcata = ();
       plugins =
         object
           method show : ('a -> string) -> ('a, _) flist -> string =
             fun fa l ->
               (* we expect no free variables here *)
               GT.show ground fa (Obj.magic l : 'a ground)
         end}
    let (%) : ('a, 'b) fancy -> ('a, 'b) flist -> ('a, 'b) flist = cons
    let (%<) : ('a, 'b) fancy -> ('a, 'b) fancy -> ('a, 'b) flist =
      fun x y -> (@@) (cons x) ((@@) (cons y) (nil ()))
    let (!<) : ('a, 'b) fancy -> ('a, 'b) flist =
      fun x -> (@@) (cons x) (nil ())
    let rec foldro f a xs r =
      conde
        [(&&&) ((===) xs (nil ())) ((===) a r);
         Fresh.three
           (fun h t a' ->
              (&&&) ((&&&) ((===) xs ((%) h t)) (f h a' r))
                (foldro f a t a'))]
    let rec mapo f xs ys =
      conde
        [(&&&) ((===) xs (nil ())) ((===) ys (nil ()));
         Fresh.two
           (fun z zs ->
              (&&&) ((===) xs ((%) z zs))
                (Fresh.two
                   (fun a1 a2 ->
                      (&&&) ((&&&) (f z a1) (mapo f zs a2))
                        ((===) ys ((%) a1 a2)))))]
    let filtero p xs ys =
      let folder x a a' =
        conde
          [(&&&) (p x Bool.true_) ((===) ((%) x a) a');
           (&&&) (p x Bool.false_) ((===) a a')]
      in
      foldro folder (nil ()) xs ys
    let rec lookupo p xs mx =
      conde
        [(&&&) ((===) xs (nil ())) ((===) mx (none ()));
         Fresh.two
           (fun h t ->
              (&&&) ((===) ((%) h t) xs)
                (conde
                   [(&&&) (p h Bool.true_) ((===) mx (some h));
                    (&&&) (p h Bool.false_) (lookupo p t mx)]))]
    let anyo = foldro Bool.oro Bool.false_
    let allo = foldro Bool.ando Bool.true_
    let rec lengtho l n =
      conde
        [(&&&) ((===) l (nil ())) ((===) n Nat.o);
         Fresh.three
           (fun x xs n' ->
              (&&&) ((&&&) ((===) l ((%) x xs)) ((===) n (Nat.s n')))
                (lengtho xs n'))]
    let rec appendo a b ab =
      conde
        [(&&&) ((===) a (nil ())) ((===) b ab);
         Fresh.three
           (fun h t ab' ->
              (&&&) ((&&&) ((===) a ((%) h t)) ((===) ((%) h ab') ab))
                (appendo t b ab'))]
    let rec reverso a b =
      conde
        [(&&&) ((===) a (nil ())) ((===) b (nil ()));
         Fresh.three
           (fun h t a' ->
              (&&&) ((&&&) ((===) a ((%) h t)) (appendo a' ((!<) h) b))
                (reverso t a'))]
    let rec membero l a =
      Fresh.two
        (fun x xs ->
           (&&&) ((===) l ((%) x xs))
             (conde [(===) x a; (&&&) ((=/=) x a) (membero xs a)]))
    let nullo q : goal = (===) q (nil ())
    let caro xs h : goal = call_fresh (fun tl -> (===) xs ((%) h tl))
    let cdro xs tl : goal = call_fresh (fun h -> (===) xs ((%) h tl))
    let hdo = caro
    let tlo = cdro
  end

let (%) = List.cons
let (%<) = List.(%<)
let (!<) = List.(!<)
let nil = List.nil

let rec inj_list : ('a, 'b) fancy list -> ('a, 'b) List.flist =
  function
    [] -> nil ()
  | x :: xs -> List.cons x (inj_list xs)

let inj_list_p xs = (@@) inj_list (List.map (fun (x, y) -> inj_pair x y) xs)

let rec inj_nat_list =
  function
    [] -> nil ()
  | x :: xs -> (%) (inj_nat x) (inj_nat_list xs)
