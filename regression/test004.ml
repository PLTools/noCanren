open MiniKanren
open Tester.M
open ImplicitPrinters

type nat = O | S of nat logic

module rec Show_nat' : SHOW with type t = nat = struct
  type t = nat
  let show = function
  | O -> "O ()"
  | S x ->
     let module S = Show_logic_explicit(Show_nat') in
     "S (" ^ (S.show x) ^ ")"
end
implicit module Show_nat = Show_nat'

open ImplicitPrinters
let (!) (x: nat) = embed x
(* let (!) {S: ImplicitPrinters.SHOW} x = embed_explicit (S.show) x *)

let rec addo x y z =
  conde [
    (x === !O) &&& (z === y);
    fresh (x' z')
       (x === !(S x'))
       (z === !(S z'))
       (addo x' y z')
  ]

(* let rec mulo x y z = *)
(*   conde [ *)
(*     (x === !O) &&& (z === !O); *)
(*     fresh (x' z') *)
(*       (x === !(S x')) *)
(*       (addo y z' z) *)
(*       (mulo x' y z') *)
(*   ] *)

open Tester



let _ =
  (* run1 ~n:1  (REPR (addo !O      !(S !O) ) ); *)
  run1 ~n:1  (REPR (addo !(S !O) !(S !O) ) );
  (* run1 ~n:2  (REPR (addo !O      !(S !O) ) ); *)
  (* run1 ~n:2  (REPR (addo !(S !O) !(S !O) ) ); *)

  (* run1 ~n:1  (REPR (fun q -> addo q !(S !O) !(S !O)            ) ); *)
  (* run1 ~n:1  (REPR (fun q -> addo !(S !O) q !(S !O)            ) ); *)
  (* run1 ~n:2  (REPR (fun q -> addo q !(S !O) !(S !O)            ) ); *)
  (* run1 ~n:2  (REPR (fun q -> addo !(S !O) q !(S !O)            ) ); *)

(*   run show_nat empty_reifier (-1)  qr  (fun q r   st -> REPR (addo q r !(S !(S !(S !(S !O))))       st), ["q", q; "r", r]); *)

(*   run1 ~n:1  (REPR (fun q     st -> REPR (mulo !O !(S !O) q                     st) ); *)
(*   run1 ~n:1  (REPR (fun q     st -> REPR (mulo !(S !(S !O)) !(S !(S !O)) q      st) ); *)
(*   run1 ~n:2  (REPR (fun q     st -> REPR (mulo !O !(S !O) q                     st) ); *)
(*   run1 ~n:1  (REPR (fun q     st -> REPR (mulo q !(S !(S !O)) !(S !(S !O))      st) ); *)
(*   run1 ~n:1  (REPR (fun q     st -> REPR (mulo q !(S !(S !O)) !(S !(S !(S !O))) st) ); *)
(*   run1 ~n:2  (REPR (fun q     st -> REPR (mulo q !(S !(S !O)) !(S !(S !O))      st) ); *)
(*   run1 ~n:2  (REPR (fun q     st -> REPR (mulo q !(S !(S !O)) !(S !(S !(S !O))) st) ); *)

(*   run1 ~n:1  (REPR (fun q     st -> REPR (mulo !(S !(S !O)) q !(S !(S !O))      st) ); *)
(*   run1 ~n:1  (REPR (fun q     st -> REPR (mulo !(S !(S !O)) q !(S !(S !(S !O))) st) ); *)
(*   run1 ~n:2  (REPR (fun q     st -> REPR (mulo !(S !(S !O)) q !(S !(S !O)) st)      ); *)
(*   run1 ~n:2  (REPR (fun q     st -> REPR (mulo !(S !(S !O)) q !(S !(S !(S !O))) st) ); *)

(*   run1 ~n:1   qr  (fun q r   st -> REPR (mulo q !(S !O) r                      st), ["q", q; "r", r]); *)
(*   run show_nat empty_reifier  10   qr  (fun q r   st -> REPR (mulo q !(S !O) r                      st), ["q", q; "r", r]); *)

(*   run1 ~n:1   qr  (fun q r   st -> REPR (mulo !(S !O) q r                      st), ["q", q; "r", r]); *)
(*   run show_nat empty_reifier  10   qr  (fun q r   st -> REPR (mulo !(S !O) q r                      st), ["q", q; "r", r]); *)

(*   run1 ~n:1   qr  (fun q r   st -> REPR (mulo q r !O                           st), ["q", q; "r", r]); *)
(*   run1 ~n:1   qr  (fun q r   st -> REPR (mulo q r !(S !O)                      st), ["q", q; "r", r]); *)

(*   run1 ~n:1  (REPR (fun q     st -> REPR (mulo !(S !O) !(S !O) q                st) ); *)
(*   run1 ~n:1   qr  (fun q r   st -> REPR (mulo q r !(S !(S !(S !(S !O))))       st), ["q", q; "r", r]); *)
(*   run1 ~n:3   qr  (fun q r   st -> REPR (mulo q r !(S !(S !(S !(S !O))))       st), ["q", q; "r", r]); *)

(*   run1 ~n:3  qrs  (fun q r s st -> REPR (mulo q r s                            st), ["q", q; "r", r; "s", s]); *)
(*   run show_nat empty_reifier  10  qrs  (fun q r s st -> REPR (mulo q r s                            st), ["q", q; "r", r; "s", s]); *)
  ()
