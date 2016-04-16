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

let rec mulo x y z =
  conde [
    (x === !O) &&& (z === !O);
    fresh (x' z')
      (x === !(S x'))
      (addo y z' z)
      (mulo x' y z')
  ]

open Tester



let _ =
  run1 ~n:1  (REPR (addo !O      !(S !O) ) );
  run1 ~n:1  (REPR (addo !(S !O) !(S !O) ) );
  run1 ~n:2  (REPR (addo !O      !(S !O) ) );
  run1 ~n:2  (REPR (addo !(S !O) !(S !O) ) );

  run1 ~n:1  (REPR (fun q -> addo q !(S !O) !(S !O)            ) );
  run1 ~n:1  (REPR (fun q -> addo !(S !O) q !(S !O)            ) );
  run1 ~n:2  (REPR (fun q -> addo q !(S !O) !(S !O)            ) );
  run1 ~n:2  (REPR (fun q -> addo !(S !O) q !(S !O)            ) );

  run2 ~n:(-1) (REPR (fun q r -> addo q r !(S !(S !(S !(S !O)))) ) );

  run1 ~n:1    (REPR (fun q     -> mulo !O !(S !O) q                     ) );
  run1 ~n:1    (REPR (fun q     -> mulo !(S !(S !O)) !(S !(S !O)) q      ) );
  run1 ~n:2    (REPR (fun q     -> mulo !O !(S !O) q                     ) );
  run1 ~n:1    (REPR (fun q     -> mulo q !(S !(S !O)) !(S !(S !O))      ) );
  run1 ~n:1    (REPR (fun q     -> mulo q !(S !(S !O)) !(S !(S !(S !O))) ) );
  run1 ~n:2    (REPR (fun q     -> mulo q !(S !(S !O)) !(S !(S !O))      ) );
  run1 ~n:2    (REPR (fun q     -> mulo q !(S !(S !O)) !(S !(S !(S !O))) ) );

  run1 ~n:1    (REPR (fun q     -> mulo !(S !(S !O)) q !(S !(S !O))      ) );
  run1 ~n:1    (REPR (fun q     -> mulo !(S !(S !O)) q !(S !(S !(S !O))) ) );
  run1 ~n:2    (REPR (fun q     -> mulo !(S !(S !O)) q !(S !(S !O))      ) );
  run1 ~n:2    (REPR (fun q     -> mulo !(S !(S !O)) q !(S !(S !(S !O))) ) );

  run2 ~n:1    (REPR (fun q r   -> mulo q !(S !O) r                    ) );
  run2 ~n:10   (REPR (fun q r   -> mulo q !(S !O) r                    ) );

  run2 ~n:1    (REPR (fun q r   -> mulo !(S !O) q r                    ) );
  run2 ~n:10   (REPR (fun q r   -> mulo !(S !O) q r                    ) );

  run2 ~n:1    (REPR (fun q r   -> mulo q r !O                         ) );
  run2 ~n:1    (REPR (fun q r   -> mulo q r !(S !O)                    ) );

  run1 ~n:1    (REPR (fun q     -> mulo !(S !O) !(S !O) q              ) );
  run2 ~n:1    (REPR (fun q r   -> mulo q r !(S !(S !(S !(S !O))))     ) );
  run2 ~n:3    (REPR (fun q r   -> mulo q r !(S !(S !(S !(S !O))))     ) );

  run3 ~n:3    (REPR (fun q r s -> mulo q r s                          ) );
  run3 ~n:10   (REPR (fun q r s -> mulo q r s                          ) );
  ()
