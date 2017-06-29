open GT
open MiniKanren
open MiniKanrenStd
open Tester

let show_nat_list = GT.(show List.ground @@ show Nat.ground)
let show_nat      = GT.(show Nat.ground)

(* Relational minimum/maximum (for nats only) *)
let minmaxo a b min max = Nat.(
    conde
      [ (min === a) &&& (max === b) &&& (a <= b)
      ; (max === a) &&& (min === b) &&& (a >  b)
      ]
  )


let runB n = runR ManualReifiers.bool GT.(show bool) (GT.show Bool.logic) n
let runN n = runR Nat.reify GT.(show Nat.ground) (GT.show Nat.logic) n

let () = Nat.(
    runB  (-1)   q  qh (REPR (fun q   -> leo (inj_nat 1) (inj_nat 2) q ));
    runB  (-1)   q  qh (REPR (fun q   -> leo (inj_nat 2) (inj_nat 1) q ));
    runB  (-1)   q  qh (REPR (fun q   -> gto (inj_nat 1) (inj_nat 2) q ));
    runB  (-1)   q  qh (REPR (fun q   -> gto (inj_nat 2) (inj_nat 1) q ));

    runN  (-1)  qr qrh (REPR (fun q r -> minmaxo (inj_nat 1) (inj_nat 2)  q r ));
    ()
  )
