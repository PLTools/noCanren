open MiniKanren
open ImplicitPrinters

type nat = O | S of nat logic

module rec Show_nat' : SHOW with type t = nat = struct
  type t = nat
  let show = function
  | O -> "O"
  | S x ->
     let module S = Show_logic_explicit(Show_nat') in
     "S" ^ (S.show x)
end
open Tester.M

let rec addo x y z =
  conde [
    (x === !O) &&& (z === y);
    fresh (x' z')
       (x === !(S x'))
       (z === !(S z'))
       (addo x' y z')
  ]

let rec mulo x y z =
  conde [
    (x === !O) &&& (z === !O);
    fresh (x' z')
      (x === !(S x'))
      (addo y z' z)
      (mulo x' y z')
  ]

let show_nat = GT.( show logic (show nat) )

open Tester
open ImplicitPrinters

let _ =
  run show_nat empty_reifier   1    q  (fun q     st -> REPR (addo !O !(S !O) q                     st), ["q", q]);
  run show_nat empty_reifier   1    q  (fun q     st -> REPR (addo !(S !O) !(S !O) q                st), ["q", q]);
  run show_nat empty_reifier   2    q  (fun q     st -> REPR (addo !O !(S !O) q                     st), ["q", q]);
  run show_nat empty_reifier   2    q  (fun q     st -> REPR (addo !(S !O) !(S !O) q                st), ["q", q]);
  run show_nat empty_reifier   1    q  (fun q     st -> REPR (addo q !(S !O) !(S !O)                st), ["q", q]);
  run show_nat empty_reifier   1    q  (fun q     st -> REPR (addo !(S !O) q !(S !O)                st), ["q", q]);
  run show_nat empty_reifier   2    q  (fun q     st -> REPR (addo q !(S !O) !(S !O)                st), ["q", q]);
  run show_nat empty_reifier   2    q  (fun q     st -> REPR (addo !(S !O) q !(S !O)                st), ["q", q]);
  run show_nat empty_reifier (-1)  qr  (fun q r   st -> REPR (addo q r !(S !(S !(S !(S !O))))       st), ["q", q; "r", r]);

  run show_nat empty_reifier   1    q  (fun q     st -> REPR (mulo !O !(S !O) q                     st), ["q", q]);
  run show_nat empty_reifier   1    q  (fun q     st -> REPR (mulo !(S !(S !O)) !(S !(S !O)) q      st), ["q", q]);
  run show_nat empty_reifier   2    q  (fun q     st -> REPR (mulo !O !(S !O) q                     st), ["q", q]);
  run show_nat empty_reifier   1    q  (fun q     st -> REPR (mulo q !(S !(S !O)) !(S !(S !O))      st), ["q", q]);
  run show_nat empty_reifier   1    q  (fun q     st -> REPR (mulo q !(S !(S !O)) !(S !(S !(S !O))) st), ["q", q]);
  run show_nat empty_reifier   2    q  (fun q     st -> REPR (mulo q !(S !(S !O)) !(S !(S !O))      st), ["q", q]);
  run show_nat empty_reifier   2    q  (fun q     st -> REPR (mulo q !(S !(S !O)) !(S !(S !(S !O))) st), ["q", q]);

  run show_nat empty_reifier   1    q  (fun q     st -> REPR (mulo !(S !(S !O)) q !(S !(S !O))      st), ["q", q]);
  run show_nat empty_reifier   1    q  (fun q     st -> REPR (mulo !(S !(S !O)) q !(S !(S !(S !O))) st), ["q", q]);
  run show_nat empty_reifier   2    q  (fun q     st -> REPR (mulo !(S !(S !O)) q !(S !(S !O)) st)     , ["q", q]);
  run show_nat empty_reifier   2    q  (fun q     st -> REPR (mulo !(S !(S !O)) q !(S !(S !(S !O))) st), ["q", q]);

  run show_nat empty_reifier   1   qr  (fun q r   st -> REPR (mulo q !(S !O) r                      st), ["q", q; "r", r]);
  run show_nat empty_reifier  10   qr  (fun q r   st -> REPR (mulo q !(S !O) r                      st), ["q", q; "r", r]);

  run show_nat empty_reifier   1   qr  (fun q r   st -> REPR (mulo !(S !O) q r                      st), ["q", q; "r", r]);
  run show_nat empty_reifier  10   qr  (fun q r   st -> REPR (mulo !(S !O) q r                      st), ["q", q; "r", r]);

  run show_nat empty_reifier   1   qr  (fun q r   st -> REPR (mulo q r !O                           st), ["q", q; "r", r]);
  run show_nat empty_reifier   1   qr  (fun q r   st -> REPR (mulo q r !(S !O)                      st), ["q", q; "r", r]);

  run show_nat empty_reifier   1    q  (fun q     st -> REPR (mulo !(S !O) !(S !O) q                st), ["q", q]);
  run show_nat empty_reifier   1   qr  (fun q r   st -> REPR (mulo q r !(S !(S !(S !(S !O))))       st), ["q", q; "r", r]);
  run show_nat empty_reifier   3   qr  (fun q r   st -> REPR (mulo q r !(S !(S !(S !(S !O))))       st), ["q", q; "r", r]);

  run show_nat empty_reifier   3  qrs  (fun q r s st -> REPR (mulo q r s                            st), ["q", q; "r", r; "s", s]);
  run show_nat empty_reifier  10  qrs  (fun q r s st -> REPR (mulo q r s                            st), ["q", q; "r", r; "s", s]);
  ()
