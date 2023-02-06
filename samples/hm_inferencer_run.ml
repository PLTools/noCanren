open GT
open OCanren
open OCanren.Std
open Tester
open Hm_inferencer.HO

(*************************************************)
let rec show_gnum num =
  let rec helper = function
    | Z -> 0
    | S x -> 1 + helper x
  in
  string_of_int @@ helper num
;;

let rec show_lambda_type f = function
  | TInt -> "int"
  | TBool -> "bool"
  | TVar n -> Printf.sprintf "_.(%s)" (f n)
  | TFun (l, r) ->
    Printf.sprintf "(%s -> %s)" (show_lambda_type f l) (show_lambda_type f r)
  | TPair (l, r) ->
    Printf.sprintf "(%s * %s)" (show_lambda_type f l) (show_lambda_type f r)
;;

let myshow x = (show_lambda_type show_gnum) x

let show_glambda f1 f2 f3 = function
  | Var s -> f1 s
  | Lit l -> f2 l
  | Tuple2 (l, r) -> Printf.sprintf "(%s, %s)" (f3 l) (f3 r)
  | App (l, r) -> Printf.sprintf "(%s)(%s)" (f3 l) (f3 r)
  | Abst (x, t) -> Printf.sprintf "(fun %s -> %s)" (f1 x) (f3 t)
  | Let (x, t1, t2) -> Printf.sprintf "let %s = %s in %s" (f1 x) (f3 t1) (f3 t2)
;;

let show_gliteral f1 f2 = function
  | LInt n -> f1 n
  | LBool b -> f2 b
;;

let show_literal = show_gliteral (show int) (show bool)

let show_lliteral l =
  show logic (show_gliteral (show logic @@ show int) (show logic @@ show bool)) l
;;

let rec show_lambda l = show_glambda (show string) show_literal show_lambda l

let rec show_llambda l =
  show logic (show_glambda (show logic (show string)) show_lliteral show_llambda) l
;;

(*************************************************)

(** For high order conversion **)
let nat_type_inference p t = nat_type_inference (( === ) p) t

let run_exn eta = run_r (lambda_type_prj_exn num_prj_exn) eta

let _ =
  let abst x y = !!(Abst (x, y)) in
  let var_ x = !!(Var x) in
  let let_ x y z = !!(Let (x, y, z)) in
  let app x y = !!(App (x, y)) in
  let tuple2 x y = !!(Tuple2 (x, y)) in
  let lit x = !!(Lit x) in
  let lInt x = !!(LInt x) in
  let lBool x = !!(LBool x) in
  let term0 = abst !!"x" (var_ !!"x") in
  let term1 =
    let_
      !!"f"
      (abst !!"x" (var_ !!"x"))
      (app (var_ !!"f") (abst !!"x" (app (var_ !!"f") (var_ !!"x"))))
  in
  let term2 =
    app
      (abst !!"f" (app (var_ !!"f") (abst !!"x" (app (var_ !!"f") (var_ !!"x")))))
      (abst !!"x" (var_ !!"x"))
  in
  let term3 = tuple2 (lit (lInt !!5)) (lit (lInt !!6)) in
  let term4 = lit (lInt !!6) in
  let term5 = lit (lBool !!true) in
  run_exn myshow (-1) q qh ("typeof term0", fun q -> nat_type_inference term0 q);
  run_exn myshow (-1) q qh ("typeof term1", fun q -> nat_type_inference term1 q);
  run_exn myshow (-1) q qh ("typeof term2", fun q -> nat_type_inference term2 q);
  run_exn myshow (-1) q qh ("typeof term5", fun q -> nat_type_inference term5 q);
  run_exn myshow (-1) q qh ("typeof term4", fun q -> nat_type_inference term4 q);
  run_exn myshow (-1) q qh ("typeof term3", fun q -> nat_type_inference term3 q)
;;
