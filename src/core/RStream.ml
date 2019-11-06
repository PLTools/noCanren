(*
 * OCanren.
 * Copyright (C) 2015-2017
 * Dmitri Boulytchev, Dmitry Kosarev, Alexey Syomin, Evgeny Moiseenko
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

type 'a s =
  | Nope
  | Answ of 'a
  | Cont of 'a   * ('a -> 'a s)
  | Conj of 'a s * int * int * ('a -> 'a s)
  | Disj of 'a s * 'a s

type 'a t =
  | Nil
  | Cons of 'a * ('a t)
  | Thunk  of 'a thunk
and 'a thunk =
  unit -> 'a t

let nope         = Nope
let answ s       = Answ s
let cont a c     = Cont (a, c)
let conj s n m c = Conj (s, n, m, c)
let disj s r     = Disj (s, r)

let rec disjs = function
| [x]     -> x
| x :: xs -> disj x @@ disjs xs
| []      -> nope

let nil         = Nil
let single x    = Cons (x, Nil)
let cons x s    = Cons (x, s)
let from_fun zz = Thunk zz

let rec of_list = function
| []    -> Nil
| x::xs -> Cons (x, of_list xs)

let force = function
| Thunk zz  -> zz ()
| xs        -> xs

let rec classic_step = function
  | Nope        -> None  , None
  | Answ a      -> Some a, None
  | Cont (a, c) -> None  , Some (c a)
  | Conj (s, n, m, c) ->
    begin match classic_step s with
    | None,   None   -> None, None
    | Some a, None   -> None, Some (cont a c)
    | None,   Some s -> None, Some (conj s n m c)
    | Some a, Some s -> None, Some (disj (cont a c) (conj s n m c))
    end
  | Disj (s1, s2) ->
    match classic_step s1 with
    | a, None    -> a, Some s2
    | a, Some s1 -> a, Some (disj s2 s1)


let rec split acc = function
  | Nope           -> []
  | Answ a         -> [a, answ, acc]
  | Cont (a, c)    -> [a, c, acc]
  | Disj (s, t)    -> split acc s @ split acc t
  | Conj (s, n, m, c) -> split (fun x -> acc @@ conj x n m c) s

let split s = List.map (fun a, c, h -> a, fun a -> h @@ cont a c) @@ split (fun x -> x) s

let rec fair_step = function
  | Nope        -> None  , None
  | Answ a      -> Some a, None
  | Cont (a, c) -> None  , Some (c a)
  | Conj (s, n, 0, c) -> None, Some (disjs @@ List.map (fun a, c' -> conj (c a) n n c') @@ split s)
  | Conj (s, n, m, c) ->
    begin match fair_step s with
    | None,   None   -> None, None
    | Some a, None   -> None, Some (cont a c)
    | None,   Some s -> None, Some (conj s n (m - 1) c)
    | Some a, Some s -> None, Some (disj (cont a c) (conj s n (m - 1) c))
    end
  | Disj (s1, s2) ->
    match fair_step s1 with
    | a, None    -> a, Some s2
    | a, Some s1 -> a, Some (disj s2 s1)


let rec transform step s =
  match step s with
  | None,   None   -> nil
  | Some a, None   -> single a
  | None  , Some s -> from_fun @@ fun () -> transform step s
  | Some a, Some s -> cons a (from_fun @@ fun () -> transform step s)

let classic_transform s = transform classic_step s
let fair_transform    s = transform fair_step    s

let rec mplus xs ys =
  match xs with
  | Nil           -> force ys
  | Cons (x, xs)  -> cons x (from_fun @@ fun () -> mplus (force ys) xs)
  | Thunk   _     -> from_fun (fun () -> mplus (force ys) xs)

let rec bind s f =
  match s with
  | Nil           -> Nil
  | Cons (x, s)   -> mplus (f x) (from_fun (fun () -> bind (force s) f))
  | Thunk zz      -> from_fun (fun () -> bind (zz ()) f)

let rec msplit = function
| Nil           -> None
| Cons (x, xs)  -> Some (x, xs)
| Thunk zz      -> msplit @@ zz ()

let is_empty s =
  match msplit s with
  | Some _  -> false
  | None    -> true

let rec map f = function
| Nil          -> Nil
| Cons (x, xs) -> Cons (f x, map f xs)
| Thunk zzz    -> from_fun (fun () -> map f @@ zzz ())

let rec iter f s =
  match msplit s with
  | Some (x, s) -> f x; iter f s
  | None        -> ()

let rec filter p s =
  match msplit s with
  | Some (x, s) -> let s = filter p s in if p x then Cons (x, s) else s
  | None        -> Nil

let rec fold f acc s =
  match msplit s with
  | Some (x, s) -> fold f (f acc x) s
  | None        -> acc

let rec zip xs ys =
  match msplit xs, msplit ys with
  | None,         None          -> Nil
  | Some (x, xs), Some (y, ys)  -> Cons ((x, y), zip xs ys)
  | _                           -> invalid_arg "OCanren fatal (Stream.zip): streams have different lengths"

let hd s =
  match msplit s with
  | Some (x, _) -> x
  | None        -> invalid_arg "OCanren fatal (Stream.hd): empty stream"

let tl s =
  match msplit s with
  | Some (_, xs) -> xs
  | None         -> Nil

let rec retrieve ?(n=(-1)) s =
  if n = 0
  then [], s
  else match msplit s with
  | None          -> [], Nil
  | Some (x, s)  -> let xs, s = retrieve ~n:(n-1) s in x::xs, s

let take ?n s = fst @@ retrieve ?n s
