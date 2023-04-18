open GT
open OCanren
open OCanren.Std
open Tester
open Unify.HO

(*************************************************)

let show_number x =
  let rec nat2int = function
    | O -> 0
    | S x -> 1 + nat2int x
  in
  Printf.sprintf "%d" @@ nat2int x
;;

let show_lnumber x =
  let rec nat2int x =
    match x with
    | OCanren.Var _ -> 0, Some (show OCanren.logic (fun _ -> "") x)
    | Value O -> 0, None
    | Value (S n) ->
      let a, s = nat2int n in
      a + 1, s
  in
  match nat2int x with
  | n, None -> Printf.sprintf "%d" n
  | 0, Some s -> Printf.sprintf "%s" s
  | n, Some s -> Printf.sprintf "(%d + %s)" n s
;;

let show_llist f x =
  let rec show_list x = Printf.sprintf "[%s]" (String.concat "; " x) in
  let rec show_llist x =
    let open List in
    match x with
    | OCanren.Var _ -> [], Some (show logic (fun _ -> "") x)
    | Value Nil -> [], None
    | Value (Cons (x, xs)) ->
      let l, q = show_llist xs in
      f x :: l, q
  in
  match show_llist x with
  | [], None -> "[]"
  | [], Some s -> s
  | x, None -> show_list x
  | x, Some s -> Printf.sprintf "%s ^ %s" (show_list x) s
;;

let rec show_gterm f g = function
  | Var v -> Printf.sprintf "V %s" (f v)
  | Constr (n, a) -> Printf.sprintf "C %s %s" (f n) (g a)
;;

let rec show_term f x = show_gterm f (show List.ground (show_term f)) x

let rec show_lterm f x =
  show OCanren.logic (show_gterm f (show List.logic (show_lterm f))) x
;;

let my_show x = show List.ground (show_term show_number) x
let my_lshow x = show_llist (show_lterm show_lnumber) x
let nat_reifier x = nat_reify x
let term_reifier x = term_reify x
let my_reifier = List.reify term_reifier
let full_run = run_r my_reifier my_lshow

(*************************************************)

let rec int2nat n = if n <= 0 then !!O else !!(S (int2nat (n - 1)))
let v n = !!(Var (int2nat n))
let c n a = !!(Constr (int2nat n, Std.list Fun.id a))
let t1 = c 0 []
let t2 = v 0
let t2' = c 0 [ t2 ]
let t3 = c 0 [ v 0; c 1 [] ]
let t4 = c 0 [ c 1 []; v 0 ]
let t5 = c 0 [ c 1 []; v 1 ]
let x1 = c 0 [ v 0; c 1 [ v 2; c 3 [] ] ]
let x2 = c 0 [ c 1 [ c 2 []; v 3 ]; v 1 ]
let x1' = c 0 [ v 0; v 0; c 1 [ v 2; c 3 [] ] ]
let x2' = c 0 [ v 1; c 1 [ c 2 []; v 3 ]; v 1 ]
let y1 = c 0 [ v 0; v 1; v 2; v 0; v 1; v 2; v 3 ]

let y2 =
  c
    0
    [ v 1
    ; v 2
    ; v 3
    ; c 1 [ c 2 []; v 5; v 6; v 7 ]
    ; c 1 [ v 4; c 3 []; v 6; v 7 ]
    ; c 1 [ v 4; v 5; c 4 []; v 7 ]
    ; c 1 [ v 4; v 5; v 6; c 5 [] ]
    ]
;;

let w1, w2 =
  let appendo x y z = c 1 [ x; y; z ] in
  let cons x y = c 2 [ x; y ] in
  let nil = c 3 [] in
  let list2 x y = cons x (cons y nil) in
  let cA = c 4 [] in
  let cB = c 5 [] in
  let cC = c 6 [] in
  let cD = c 7 [] in
  let vX = v 0 in
  let vXs = v 1 in
  let vYs = v 2 in
  let vZs = v 3 in
  let vLs = v 4 in
  ( appendo (list2 cA cB) (list2 cC cD) vLs
  , (* append([a, b]  , [c, d], Ls      ) *)
    appendo (cons vX vXs) vYs (cons vX vZs) )
;;

(* append([X | Xs], Ys    , [X | Zs]) *)

(** For high order conversion **)
let check_uni q t1 t2 r = check_uni (( === ) q) (( === ) t1) (( === ) t2) r

let _ =
  full_run (-1) q qh ("answers1", fun q -> check_uni q t1 t1 !!true);
  full_run (-1) q qh ("answers2", fun q -> check_uni q t2 t2 !!true);
  full_run (-1) q qh ("answers3", fun q -> check_uni q t1 t2 !!true);
  full_run (-1) q qh ("answers4", fun q -> check_uni q t3 t4 !!true);
  full_run (-1) q qh ("answers5", fun q -> check_uni q t3 t5 !!true);
  full_run (-1) q qh ("answers6", fun q -> check_uni q t4 t5 !!true);
  full_run (-1) q qh ("answers7", fun q -> check_uni q x1 x2 !!true);
  full_run (-1) q qh ("answers8", fun q -> check_uni q x1' x2' !!true);
  full_run (-1) q qh ("answers9", fun q -> check_uni q y1 y2 !!true);
  full_run (-1) q qh ("answers_bad", fun q -> check_uni q t2 t2' !!true);
  full_run (-1) q qh ("answers", fun q -> check_uni q w1 w2 !!true);
  ()
;;
