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

type 'a t =
  | Nil
  | Cont  of 'a * ('a -> 'a t) list * 'a t
  | Thunk of 'a thunk
  (* | Waiting of 'a suspended list *)
and 'a thunk =
  unit -> 'a t
(* and 'a suspended =
  {is_ready: unit -> bool; zz: 'a thunk} *)

let nil         = Nil
let single x    = Cont (x, [], Nil)
let cons x s    = Cont (x, [], s)
let cont x ks s = Cont (x, ks, s)

let from_fun zz = Thunk zz

(* let suspend ~is_ready f = Waiting [{is_ready; zz=f}] *)

let rec of_list = function
  | []    -> nil
  | x::xs -> cons x (of_list xs)

let force = function
  | Thunk zz  -> zz ()
  | xs        -> xs

let rec mplus xs ys =
  match xs with
  | Nil               -> force ys
  | Cont (x, ks, xs)  -> cont x ks (from_fun @@ fun () -> mplus (force ys) xs)
  | Thunk   _         -> from_fun (fun () -> mplus (force ys) xs)
  (* | Waiting ss    ->
    let ys = force ys in
    (* handling waiting streams is tricky *)
    match unwrap_suspended ss, ys with
    (* if [xs] has no ready streams and [ys] is also a waiting stream then we merge them  *)
    | Waiting ss, Waiting ss' -> Waiting (ss @ ss')
    (* if [xs] has no ready streams but [ys] is not a waiting stream then we swap them,
       pushing waiting stream to the back of the new stream *)
    | Waiting ss, _           -> mplus ys @@ from_fun (fun () -> xs)
    (* if [xs] has ready streams then [xs'] contains some lazy stream that is ready to produce new answers *)
    | xs', _ -> mplus xs' ys *)

(* and unwrap_suspended ss =
  let rec find_ready prefix = function
    | ({is_ready; zz} as s)::ss ->
      if is_ready ()
      then Some (from_fun zz), (List.rev prefix) @ ss
      else find_ready (s::prefix) ss
    | [] -> None, List.rev prefix
  in
  match find_ready [] ss with
    | Some s, [] -> s
    | Some s, ss -> mplus (force s) @@ Waiting ss
    | None , ss  -> Waiting ss *)

(* TODO: better name *)
let rec fbind : 'a t -> ('a -> 'a t) -> 'a t = fun s k ->
  match s with
  | Nil                 -> Nil
  | Cont (x, ks, s)     -> Cont (x, k::ks, fbind s k)
  | Thunk zz            -> from_fun (fun () -> fbind (zz ()) k)

(* TODO: better name *)
let rec enforce s fs =
  match s with
  | Nil -> Nil
  | Cont (x, ks, s) ->
    let fx = match ks @ fs with
      | []    -> single x
      | k::ks -> enforce (k x) ks
    in mplus fx (from_fun @@ fun () -> enforce s fs)
  | Thunk zz -> from_fun @@ fun () -> enforce (zz ()) fs

let bind s f =
  let rec bind s f =
    match s with
    | Nil                 -> Nil
    | Cont (x, [], s)     -> mplus (f x) (from_fun (fun () -> bind (force s) f))
    | Thunk zz            -> from_fun (fun () -> bind (zz ()) f)
  in bind (enforce s []) f

  (* | Waiting ss    ->
    match unwrap_suspended ss with
    | Waiting ss ->
      let helper {zz} as s = {s with zz = fun () -> bind (zz ()) f} in
      Waiting (List.map helper ss)
    | s          -> bind s f *)

let map f s =
  let rec map f = function
  | Nil             -> Nil
  | Cont (x, [], s) -> cons (f x) @@ map f s
  | Thunk zz        -> from_fun (fun () -> map f @@ zz ())
  in map f @@ enforce s []

let filter p s =
  let rec filter p = function
  | Nil             -> Nil
  | Cont (x, [], s) ->
    let s = filter p s in
    if p x then cons x s else s
  | Thunk zz        -> from_fun (fun () -> filter p @@ zz ())
  in filter p @@ enforce s []

  (* | Waiting ss   ->
    let helper {zz} as s = {s with zz = fun () -> map f (zz ())} in
    Waiting (List.map helper ss) *)

let msplit s =
  let rec msplit = function
  | Nil               -> None
  | Cont (x, [], xs)  -> Some (x, xs)
  | Thunk zz          -> msplit @@ zz ()
  in msplit @@ enforce s []
(* | Waiting ss    ->
  match unwrap_suspended ss with
  | Waiting _ -> None
  | xs        -> msplit xs *)

let is_empty s =
  match msplit s with
  | Some _  -> false
  | None    -> true

let rec iter f s =
  match msplit s with
  | Some (x, s) -> f x; iter f s
  | None        -> ()

let rec fold f acc s =
  match msplit s with
  | Some (x, s) -> fold f (f acc x) s
  | None        -> acc

let rec zip xs ys =
  match msplit xs, msplit ys with
  | None,         None          -> Nil
  | Some (x, xs), Some (y, ys)  -> cons (x, y) (zip xs ys)
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
