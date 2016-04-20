open Printf
open MiniKanren
open Tester.M
open ImplicitPrinters

let (!) : {S: ImplicitPrinters.SHOW} -> S.t -> S.t logic = embed

let lookupo {Key: SHOW} {Value: SHOW} (a: Key.t logic) g (t: Value.t logic) =
  (* printf "=== lookupo ~a:'%s' ~g:'%s' ~t:'%s'\n%!" *)
  (*        (show_logic_naive a) (show_logic_naive g)  (show_logic_naive t); *)
  let rec helper a g t =
    fresh (a' t' tl)
          (g ===
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
module Show_env_explicit = Show_logic_explicit(
          MiniKanren.Show_llist_explicit(Show_pair_explicit(Show_string_logic)(Show_typ_logic)) )
open Tester

let _ =
  (* run1 ~n:1    (REPR (lookupo (!"x") (of_list ([]: (string logic * typ logic) list)) ) ); *)
  (* (\* run2 ~n:1    (REPR (fun q r -> (of_list [!"x", !(V !"x")]) === of_list [ (q,r) ] ) ); *\) *)

  (* run1 (\* ~printer:Show_typ_logic.show *\) ~n:1 *)
  (*      (REPR (lookupo !"x" (of_list [!"x", !(V !"x")])            ) ); *)
  (* run1 (\* ~printer:Show_typ_logic.show *\) ~n:1 *)
  (*      (REPR (fun q -> lookupo !"x" (of_list [!"y", !(V !"y"); !"x", !(V !"x")]) q) ); *)

  (* run1 (\* ~printer:Show_string_logic.show *\) ~n:1 *)
  (*      (REPR (fun q -> lookupo q (of_list [!"y", !(V !"y"); !"x", !(V !"x")]) !(V !"x")     ) ); *)
  (* run1 (\* ~printer:Show_env_explicit.show *\) ~n:1 *)
  (*      (REPR(fun q -> lookupo !"x" q !(V !"y")                                             ) ); *)

  (* run1 ~n:1 (REPR(infero !(Abs (!"x", !(X !"x")))                                    ) ); *)
  (* run1 ~n:1 (REPR(infero !(Abs (!"f", !(Abs (!"x", !(App (!(X !"f"), !(X !"x"))))))) ) ); *)
  (* run1 ~n:1 (REPR(infero !(Abs (!"x", !(Abs (!"f", !(App (!(X !"f"), !(X !"x"))))))) ) ); *)
  (* run1 ~n:2 (REPR(fun q -> infero q !(Arr (!(V !"x"), !(V !"x")))                               ) ); *)
  (* run1 ~n:1 (REPR(infero !(Abs (!"x", !(App (!(X !"x"), !(X !"x")))))                ) ); *)
  ()
(*
let rec substo l x a l' =
    fresh (b)
      (1)
      (conde [
         (* (x  === v) ; *)
         fresh (b') (l' === b')
       ])
    *)

let rec substo l x a l' =
  conde [
    fresh (y) (l === !(X y))(y === x)(l' === a);
    fresh (m n m' n')
       (l  === !(App (m, n)))
       (l' === !(App (m', n')))
       (substo m x a m')
       (substo n x a n');
    fresh (v b)
      (l === !(Abs (v, b)))
      (conde [
         (x  === v) &&& (l' === l);
         fresh (b') (l' === !(Abs (v, b'))) (substo b x a b')
       ])
  ]

let rec evalo m n =
  conde [
    fresh (x)
      (m === !(X x))
      (n === m);
    fresh (x l)
      (m === !(Abs (x, l)))
      (n === m);
    fresh (f a f' a')
      (m === !(App (f, a)))
      (conde [
         fresh (x l l')
           (f' === !(Abs (x, l)))
           (substo l x a' l')
           (evalo l' n);
         fresh (p q) (f' === !(App (p, q))) (n === !(App (f', a')));
         fresh (x) (f' === !(X x)) (n === !(App (f', a')))
       ])
      (evalo f f')
      (evalo a a')
  ]

let () =
  run1 ~n:1  (REPR (substo !(X !"x") !"x" !(X !"y") ) );
  run1 ~n:1  (REPR(fun q   -> evalo !(Abs (!"x", !(X !"x"))) q                    ) );
  run1 ~n:2  (REPR(fun q   -> evalo !(Abs (!"x", !(X !"x"))) q                    ) );
  run1 ~n:1  (REPR(fun q   -> evalo !(App (!(Abs (!"x", !(X !"x"))), !(X !"y"))) q) );
  run1 ~n:1  (REPR(fun q   -> evalo !(App (!(Abs (!"x", !(X !"x"))), q)) !(X !"y")) );
  run1 ~n:1  (REPR(fun q   -> evalo !(App (!(Abs (!"x", q)), !(X !"y"))) !(X !"y")) );
  run1 ~n:1  (REPR(fun q   -> evalo !(App (q, !(X !"x"))) !(X !"x")               ) );
  run1 ~n:1  (REPR(fun q   -> evalo !(App (!(X !"x"), !(X !"x"))) q               ) );
  run1 ~n:1  (REPR(fun q   -> evalo !(X !"x") q                                   ) );
  run2 ~n:1  (REPR(fun q r -> evalo !(App (r, q)) !(X !"x")                       ) );
  ()
