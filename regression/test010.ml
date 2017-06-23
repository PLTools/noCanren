(* Some tests about constraints *)
open GT
open MiniKanren
open Tester

let (!) = (!!)
let g123 x   = conde [x === !1; x === !2; x === !3]
let g12  x   = (g123 x) &&& (x =/= !3)
let gxy  x y = (g123 x) &&& (g123 y)
let gxy' x y = (gxy x y) &&& (x =/= y)
let gnot5 x  = x =/= !5

let show_int = show(int)

let runI n = runR ManualReifiers.int_reifier show_int (show(logic) show_int) n

(* let (===) ?loc = unitrace ?loc (fun h t -> GT.(show logic @@ show int) @@ ManualReifiers.int_reifier h t)
let (=/=) = diseqtrace  (fun h t -> GT.(show logic @@ show int) @@ ManualReifiers.int_reifier h t) *)

let _ =
  runI    3    q   qh (REPR (fun q   -> g123 q                                                    ));
  runI    3    q   qh (REPR (fun q   -> g12 q                                                     ));
  runI   10   qr  qrh (REPR (fun q r -> gxy q r                                                   ));
  runI   10   qr  qrh (REPR (fun q r -> gxy' q r                                                  ));
  runI (-1)    q   qh (REPR (fun q   -> (fresh (x y) (x === y)(x =/= y))                          ));
  runI (-1)    q   qh (REPR (fun q   -> (fresh (x y) (x =/= y)(x === y))                          ));
  runI (-1)    q   qh (REPR (fun q   -> (fresh (x y) (x =/= y)(!3 === x)(!3 === y))               ));
  runI (-1)    q   qh (REPR (fun q   -> (fresh (x y) (!3 === x)(x =/= x)(!3 === y))               ));
  runI (-1)    q   qh (REPR (fun q   -> (fresh (x y) (!3 === x)(!3 === y)(x =/= y))               ));
  runI (-1)    q   qh (REPR (fun q   -> (fresh (x y) (!3 === x)(!3 === y)(y =/= x))               ));
  runI (-1)    q   qh (REPR (fun q   -> (fresh (x y z) (x === y)(y === z)(x =/= !4)(z === !(2+2)))));
  runI (-1)    q   qh (REPR (fun q   -> (fresh (x y z) (x === y)(y === z)(z === !(2+2))(x =/= !4))));
  runI (-1)    q   qh (REPR (fun q   -> (fresh (x y z) (x =/= !4)(y === z)(x === y)(z === !(2+2)))));
  ()


let _ =
  runI (-1)  q qh (REPR (fun q   -> (q =/= !5)                                                ));
  runI (-1)  q qh (REPR (fun q   -> ((q =/= !3) &&& (q === !3))                               ));
  runI (-1)  q qh (REPR (fun q   -> ((q === !3) &&& (!3 =/= q))                               ))
