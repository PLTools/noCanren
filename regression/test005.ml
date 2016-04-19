open Printf
open MiniKanren
open Tester.M
open ImplicitPrinters

let (!) : {S: ImplicitPrinters.SHOW} -> S.t -> S.t logic = embed
(*
let lookupo {Key: SHOW} {Value: SHOW}  g  =
  printf "lookupo ~a:'%s' ~g:'%s' ~t:'%s'\n%!" (show_logic_naive a) "?" (show_logic_naive t);
  let rec helper  g  =
    fresh (x y)
          (g === y)
  in
  helper g
 *)

let lookupo {Key: SHOW} {Value: SHOW} (a: Key.t logic) g (t: Value.t logic) =
  (* printf "=== lookupo ~a:'%s' ~g:'%s' ~t:'%s'\n%!" *)
  (*        (show_logic_naive a) (show_logic_naive g)  (show_logic_naive t); *)
  let rec helper a g t =
    fresh (a' t' tl)
          (g ===
             (* (embed_explicit (fun (x,y) -> sprintf "(%s,%s)" *)
             (*                                       (show_logic_naive x) (show_logic_naive y)) *)
             (*                 (a', t')) *)
             ! ( (a' : Key.t logic), (t': Value.t logic) )
             % tl)
          (conde [
               (a' === a) &&& (t' === t);
               helper a tl t
             ])
  in
  helper a g t


type lam = X of string logic | App of lam logic * lam logic | Abs of string logic * lam logic

module rec Show_lam : SHOW with type t = lam = struct
  type t = lam
  let show = function
    | X strl when is_value strl -> sprintf "X (%s)" (to_value_exn strl)
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
    | V strl when is_value strl -> sprintf "V (%s)" (to_value_exn strl)
    | V strl -> sprintf "V (%s)" (show_logic_naive strl)
    (* | V strl -> sprintf "V (%s)" (show_logic_naive strl) *)
    | Arr (t1, t2) ->
       sprintf "Arr (%s, %s)" (Show_typ_logic.show t1)  (Show_typ_logic.show t2)
end
and Show_typ_logic: SHOW with type t = typ logic = Show_logic_explicit(Show_typ)

implicit module Show_typ_implicit = Show_typ


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

module Show_string_logic = MiniKanren.Show_logic_explicit(Show_string)
module Show_env_explicit = Show_logic_explicit( MiniKanren.Show_llist_explicit(Show_pair_explicit(Show_string_logic)(Show_typ_logic)) )
open Tester

let _ =
  run1 ~n:1    (REPR (lookupo (!"x") (of_list ([]: (string logic * typ logic) list)) ) );
  (* run2 ~n:1    (REPR (fun q r -> (of_list [!"x", !(V !"x")]) === of_list [ (q,r) ] ) ); *)

  run1 (* ~printer:Show_typ_logic.show *) ~n:1
       (REPR (lookupo !"x" (of_list [!"x", !(V !"x")])            ) );
  run1 (* ~printer:Show_typ_logic.show *) ~n:1
       (REPR (fun q -> lookupo !"x" (of_list [!"y", !(V !"y"); !"x", !(V !"x")]) q) );

  run1 (* ~printer:Show_string_logic.show *) ~n:1
       (REPR (fun q -> lookupo q (of_list [!"y", !(V !"y"); !"x", !(V !"x")]) !(V !"x")     ) );
  run1 (* ~printer:Show_env_explicit.show *) ~n:1
       (REPR(fun q -> lookupo !"x" q !(V !"y")                                             ) );

  run1 ~n:1 (REPR(infero !(Abs (!"x", !(X !"x")))                                    ) );
  run1 ~n:1 (REPR(infero !(Abs (!"f", !(Abs (!"x", !(App (!(X !"f"), !(X !"x"))))))) ) );
  run1 ~n:1 (REPR(infero !(Abs (!"x", !(Abs (!"f", !(App (!(X !"f"), !(X !"x"))))))) ) );
  run1 ~n:2 (REPR(fun q -> infero q !(Arr (!(V !"x"), !(V !"x")))                               ) );
  run1 ~n:1 (REPR(infero !(Abs (!"x", !(App (!(X !"x"), !(X !"x")))))                ) );
  ()
