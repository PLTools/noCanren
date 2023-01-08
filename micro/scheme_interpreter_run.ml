open GT
open OCanren
open OCanren.Std
open Tester
open Scheme_interpreter

(*************************************************)

let rec var_reify x = For_gvariable.reify var_reify x
let ident_reify x = For_gidentifier.reify var_reify x
let rec term_reify x = For_gterm.reify ident_reify (List.reify term_reify) x

let show_var f = function
  | First -> "F"
  | Next x -> Printf.sprintf "N(%s)" (f x)
;;

let show_ident f = function
  | Lambda -> "Lambda"
  | Quote -> "Quote"
  | List -> "List"
  | Var x -> Printf.sprintf "Var (%s)" (f x)
;;

let show_term f g = function
  | Ident x -> f x
  | Seq y -> g y
;;

let rec show_var' x = show_var show_var' x
let show_ident' x = show_ident show_var' x
let rec show_term' x = show_term show_ident' (show List.ground show_term') x
let rec show_lvar x = show logic (show_var show_lvar) x
let show_lindent x = show logic (show_ident show_lvar) x
let rec show_lterm x = show logic (show_term show_lindent (show List.logic show_lterm)) x
let run x = runR term_reify show_term' show_lterm x

(*************************************************)

(** For high order conversion **)
let eval q l r = eval (( === ) q) (( === ) l) r

(** For call-by-need conversion **)
(* let eval q l r = (snd eval) ([Obj.magic q], (===) q) ([], (===) l) r *)

let _ = run 1 q qh ("test", fun q -> eval q (nil ()) (val_ q))
