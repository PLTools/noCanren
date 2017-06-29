open Printf
open MiniKanren
open MiniKanrenStd
open Tester

let show_nat        = GT.show(Nat.ground)
let show_bool       = GT.show(Bool.ground)

let show_nat_list  = GT.show(List.ground) (GT.show(Nat.ground))
let show_bool_list = GT.show(List.ground) (GT.show(Bool.ground))
let show_lbool_llist = GT.(show List.logic @@ GT.show Bool.logic)
let show_nat_option = GT.(show option  (show Nat.ground))
let show_lnat_loption = GT.(show logic @@ show option (show Nat.logic))

let (?$) = inj_nat
let nats = inj_nat_list
let bools bs = inj_listi @@ List.map (!!) bs

let sumo = List.foldro Nat.addo ?$0

let show_nat_list   = GT.(show List.ground @@ show Nat.ground)
let show_lnat_llist = GT.(show List.logic  @@ show Nat.logic)

let runB n = runR ManualReifiers.bool show_bool (GT.show Bool.logic) n
let runN n = runR Nat.reify show_nat (GT.show Nat.logic) n
let runL n = runR (List.reify Nat.reify) show_nat_list show_lnat_llist n
let runBL n = runR (List.reify ManualReifiers.bool) show_bool_list show_lbool_llist n
let runNO n = runR (Option.reify Nat.reify) show_nat_option show_lnat_loption n

let () =
  runB                     1    q  qh (REPR (fun q     -> Bool.noto' Bool.true_  q                       ));
  runB                     1    q  qh (REPR (fun q     -> Bool.noto' Bool.false_ q                       ));
  runB                     1    q  qh (REPR (fun q     -> Bool.noto' q          Bool.true_               ));
  runB                     1    q  qh (REPR (fun q     -> Bool.oro  Bool.false_ Bool.false_ q            ));
  runB                     1    q  qh (REPR (fun q     -> Bool.oro  Bool.false_ Bool.true_  q            ));
  runB                     1    q  qh (REPR (fun q     -> Bool.oro  Bool.true_  Bool.false_ q            ));
  runB                     1    q  qh (REPR (fun q     -> Bool.oro  Bool.true_  Bool.true_  q            ));
  runB                     1    q  qh (REPR (fun q     -> Bool.ando Bool.false_ Bool.false_ q            ));
  runB                     1    q  qh (REPR (fun q     -> Bool.ando Bool.false_ Bool.true_  q            ));
  runB                     1    q  qh (REPR (fun q     -> Bool.ando Bool.true_  Bool.false_ q            ));
  runB                     1    q  qh (REPR (fun q     -> Bool.ando Bool.true_  Bool.true_  q            ));
  runN                     1    q  qh (REPR (fun q     -> Nat.addo ?$0 ?$1 q                             ));
  runN                     1    q  qh (REPR (fun q     -> Nat.addo ?$1 q   ?$3                           ));
  runN                     3   qr qrh (REPR (fun q r   -> Nat.addo q   r   q                             ));
  runN                     1    q  qh (REPR (fun q     -> Nat.mulo ?$1 ?$2 q                             ));
  runN                     1    q  qh (REPR (fun q     -> Nat.mulo ?$3 q   ?$6                           ));
  runN                     1    q  qh (REPR (fun q     -> Nat.mulo ?$3 q   ?$6                           ));
  runN                     1    q  qh (REPR (fun q     -> Nat.mulo ?$3 ?$0 q                             ));
  runN                     1    q  qh (REPR (fun q     -> Nat.mulo q   ?$5 ?$0                           ));
  runN                     3    q  qh (REPR (fun q     -> Nat.mulo q   ?$0 ?$0                           ))

let () =
  runN                     1    q  qh (REPR (fun q     -> sumo (nats []) q                               ));
  runN                     1    q  qh (REPR (fun q     -> sumo (nats [3;1;2]) q                          ));
  runN                     1    q  qh (REPR (fun q     -> sumo (?$0 % (?$1 % (q %< ?$3))) ?$6            ));
  ()

let () =
  runN                     1    q   qh (REPR (fun q     -> List.lengtho (nats [1;2;3;4]) q                    ));
  runN                     1    q   qh (REPR (fun q     -> List.lengtho (inj_listi [!!(); !!(); !!()]) q    ));
  runN                     1    q   qh (REPR (fun q     -> List.lengtho (bools [false; true]) q               ));
  runN                     1    q   qh (REPR (fun q     -> List.lengtho (nats [4;3;2;1;0]) q                  ));
  runL                     1    q   qh (REPR (fun q     -> List.lengtho q ?$0                                 ));

  runB                     1    q   qh (REPR (fun q     -> List.anyo (bools [false;false;true]) q         ));
  runB                     1    q   qh (REPR (fun q     -> List.anyo (bools [false;false]) q              ));

  runB                     1    q   qh (REPR (fun q     -> List.allo (bools [true;false;true]) q          ));
  runB                     1    q   qh (REPR (fun q     -> List.allo (Bool.true_ % (q %< Bool.true_)) Bool.true_  ));
  runB                   (-1) qrs qrsh (REPR (fun q r s -> List.allo (Bool.true_ % (q %< r)) s                    ));
  ()

let () =
  runL                      1    q  qh (REPR (fun q     -> List.mapo (Nat.addo ?$1) (nats [0;1;2]) q              ));
  runL                      1    q  qh (REPR (fun q     -> List.mapo (Nat.addo ?$2) q (nats [4;3;2])              ));
  runN                      1    q  qh (REPR (fun q     -> List.mapo (Nat.addo q) (nats [1;2;3]) (nats [4;5;6])   ));
  runN                      1    q  qh (REPR (fun q     -> List.mapo (Nat.mulo q) (nats [1;2;3]) (nats [2;4;6])   ));
  runN                      1   qr qrh (REPR (fun q r   -> List.mapo (Nat.mulo q) (nats [1;2]) (?$2 %< r)         ));
  runL                      1    q  qh (REPR (fun q     -> List.mapo (===) (nats [1;2;3]) q                       ));
  runN                      1    q  qh (REPR (fun q     -> List.mapo (===) (nats [1;2;3]) (?$1 % (?$2 %< q))    ));
  runBL                     1    q  qh (REPR (fun q     -> List.mapo Bool.noto' (bools [true;false;true;]) q    ));
  runBL                     1    q  qh (REPR (fun q     -> List.mapo Bool.noto' (bools []) q                    ));

  runL                    (-1)   q  qh (REPR (fun q     -> List.filtero (eqo ?$2) (nats [0;1;2;3]) q          ));
  runNO                     1    q  qh (REPR (fun q     -> List.lookupo (eqo ?$1) (nats [0;2;1;3]) q          ));
  ()

let _freeVars =
  runN         3   qr qrh (REPR (fun q r   -> Nat.mulo q   r   q             ));
  runL      (-1)    q  qh (REPR (fun q     -> List.lengtho q ?$3             ))
