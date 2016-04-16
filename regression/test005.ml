open Printf
open MiniKanren
open Tester.M
open ImplicitPrinters

let (_) = embed (Nil: string   llist)

let iter_print2 {X: SHOW} (xs: X.t list) =
  let rec inner xs =
  match xs with
  | x::xs -> print_endline (X.show x); inner xs
  | [] -> ()
  in
  inner xs

(*
let rec iter_print {X: SHOW} (xs: X.t list) =
  match xs with
  | x::xs -> print_endline (X.show x); iter_print xs
  | [] -> ()
 *)

type lam = X of string logic | App of lam logic * lam logic | Abs of string logic * lam logic

module rec Show_lam : SHOW with type t = lam = struct
  type t = lam
  let show = function
    | X strl -> sprintf "X (%s)" (show_logic_naive strl)
    | App (lam1, lam2) ->
       sprintf "App (%s, %s)" (Show_lam_logic.show lam1) (Show_lam_logic.show lam2)
    | Abs (s, lam) ->
       sprintf "Abs (%s, %s)" (show_logic_naive s) (Show_lam_logic.show lam)
end
and Show_lam_logic: SHOW with type t = lam logic = Show_logic_explicit(Show_lam)

implicit module Show_lam_implicit = Show_lam

type typ = V of string logic | Arr of typ logic * typ logic

module rec Show_typ : SHOW with type t = typ = struct
  type t = typ
  let show = function
    | V strl -> sprintf "V (%s)" (show_logic_naive strl)
    | Arr (t1, t2) ->
       sprintf "Arr (%s, %s)" (Show_typ_logic.show t1)  (Show_typ_logic.show t2)
end
and Show_typ_logic: SHOW with type t = typ logic = Show_logic_explicit(Show_typ)

implicit module Show_typ_implicit = Show_typ

implicit module Show_paired_strings = struct
  type t = string logic * string logic
  let show (x,y) =
    sprintf "(%s, %s)" (show_logic_naive x)   (show_logic_naive y)
  end

implicit module Show_string_typ = struct
  type t = string logic * typ logic
  let show (x,y) =
    sprintf "(%s, %s)" (show_logic_naive x)   (show_logic_naive y)
  end

let _f () = embed (V (embed "x"))
(* let _f () = of_list [ (embed "x", embed "x") ] *)
let _f () = of_list [ ("x", 1) ]
let (!) : {S: ImplicitPrinters.SHOW} -> S.t -> S.t logic = embed

let lookupo {Key: SHOW} {Value: SHOW} (a: Key.t logic) g (t: Value.t logic) =
  let rec helper a g t =
  fresh (a' t' tl)
   (g ===
      (embed_explicit (fun (x,y) -> sprintf "(%s,%s)"
                                            (show_logic_naive x) (show_logic_naive y))
                      (a', t'))
      % tl)
    (conde [
      (a' === a) &&& (t' === t);
      helper a tl t
     ])
  in
  helper a g t

(* let (_:int) = lookupo *)
(* let (_:int) = of_list [!"x", !(V !"x")] *)


let infero (expr: Show_lam_logic.t) (typ: Show_typ_logic.t) =
  let rec infero gamma expr typ =
    conde [
      fresh (x)
        (expr === !(X x))
        (lookupo x gamma typ);
      fresh (m n t)
        (expr === !(App (m, n)))
        (infero gamma m !(Arr (t, typ)))
        (infero gamma n t);
      fresh (x l t t')
        (expr === !(Abs (x, l)))
        (typ  === !(Arr (t, t')))
        (infero (!(x, t)%gamma) l t')
    ]
  in
  infero !(Nil: (string logic * typ logic) llist) expr typ
  (* infero !(Nil: int llist) expr typ *)

(*
(* let (_:int) = (!) *)

(* let show_env    = GT.( show logic (show llist (show pair (show logic (show string)) (show logic (show typ)))) ) *)
(* let show_typ    = GT.( show logic (show typ) ) *)
(* let show_lam    = GT.( show logic (show lam) ) *)
(* let show_string = GT.( show logic (show string) ) *)

open Tester

let _ =
  run1 ~n:1    (REPR (lookupo (!"x") (of_list ([]: (string logic * typ logic) list)) ) );
  run1 ~n:1    (REPR (lookupo !"x" (of_list [!"x", !(V !"x")])            ) );
  (* run1 ~n:1    (REPR (fun q -> lookupo !"x" (of_list [!"y", !(V !"y"); !"x", !(V !"x")]) q) ); *)

  (* run1 ~n:1    (REPR (fun q -> lookupo q (of_list [!"y", !(V !"y"); !"x", !(V !"x")]) !(V !"x")     ) ); *)
  (* run1 ~n:1    (REPR (fun q -> lookupo q (of_list [!"y", !(V !"y"); !"x", !(V !"x")]) !(V !"y")     ) ); *)
  (* run show_env    empty_reifier 1 (fun q -> lookupo !"x" q !(V !"y")                                             ) ); *)
  (* run show_env    empty_reifier 5 (fun q -> lookupo !"x" q !(V !"y")                                             ) ); *)
  (* run1 1 (fun q -> infero !(Abs (!"x", !(X !"x"))) q                                    ) ); *)
  (* run1 1 (fun q -> infero !(Abs (!"f", !(Abs (!"x", !(App (!(X !"f"), !(X !"x"))))))) q ) ); *)
  (* run1 1 (fun q -> infero !(Abs (!"x", !(Abs (!"f", !(App (!(X !"f"), !(X !"x"))))))) q ) ); *)
  (* run show_lam    empty_reifier 1 (fun q -> infero q !(Arr (!(V !"x"), !(V !"x")))                               ) ); *)
  (* run1 1 (fun q -> infero !(Abs (!"x", !(App (!(X !"x"), !(X !"x")))))                q ) ); *)
  ()
  *)
