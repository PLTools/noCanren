open GT
open OCanren
open OCanren.Std
type bottle =
  | Fst 
  | Snd 
let fst_ () = !! Fst
let snd_ () = !! Snd
type stepType =
  | Fill 
  | Empty 
  | Pour 
let fill () = !! Fill
let empty () = !! Empty
let pour () = !! Pour
type 'a0 gnat =
  | O 
  | S of 'a0 
module For_gnat = (Fmap)(struct let rec fmap fa0 = function | O -> O | S a0 -> S (fa0 a0)
                                type 'a0 t = 'a0 gnat end)
let rec o () = inj (For_gnat.distrib O)
and s x__0 = inj (For_gnat.distrib (S x__0))
let rec add a b q5 = fresh (q1) (a q1) (((q1 === (o ())) &&& (b q5)) ||| (fresh (x) (q1 === (s x)) (add (fun q4 -> x === q4) (fun q3 -> fresh (q2) (q3 === (s q2)) (b q2)) q5)))
let rec greater a b q14 =
  fresh (q7) (a q7)
    (((q7 === (o ())) &&& (q14 === (!! false))) |||
       (fresh (x q10) (q7 === (s x)) (b q10)
          (((q10 === (o ())) &&& (q14 === (!! true))) ||| (fresh (y) (q10 === (s y)) (greater (fun q13 -> x === q13) (fun q12 -> y === q12) q14)))))
let rec sub a b q22 =
  fresh (q16) (b q16)
    (((q16 === (o ())) &&& (a q22)) |||
       (fresh (y q18) (q16 === (s y)) (a q18) (((q18 === (o ())) &&& (q22 === (o ()))) ||| (fresh (x) (q18 === (s x)) (sub (fun q20 -> x === q20) (fun q21 -> y === q21) q22)))))
let anotherBottle q23 q24 = fresh (q25) (q23 q25) (((q25 === (fst_ ())) &&& (q24 === (snd_ ()))) ||| ((q25 === (snd_ ())) &&& (q24 === (fst_ ()))))
let createState bottle lvl1 lvl2 q36 =
  fresh (q29) (bottle q29)
    ((fresh (q30 q31) (q29 === (fst_ ())) (q36 === (pair q30 q31)) (lvl1 q30) (lvl2 q31)) ||| (fresh (q33 q34) (q29 === (snd_ ())) (q36 === (pair q33 q34)) (lvl2 q33) (lvl1 q34)))
let checkStep q37 q39 q38 q40 =
  fresh (q41 f s q45 t b) (q41 === (pair f s)) (q45 === (pair t b)) (
    q37 q41) (q39 q45)
    (let lvl1 q46 = fresh (q47) (b === q47) (((q47 === (fst_ ())) &&& (f === q46)) ||| ((q47 === (snd_ ())) &&& (s === q46))) in
     let lvl2 q48 = fresh (q49) (b === q49) (((q49 === (fst_ ())) &&& (s === q48)) ||| ((q49 === (snd_ ())) &&& (f === q48))) in
     fresh (q51) (t === q51)
       (conde
          [fresh (q55 q56) (q51 === (fill ())) (q56 === (o ())) (lvl1 q55) (conde [(q55 === q56) &&& (q40 === (!! true)); (q40 === (!! false)) &&& (q55 =/= q56)]);
          fresh (q60 q61) (q51 === (empty ())) (lvl1 q60) (q38 (fun q83 -> b === q83) q61) (conde [(q60 === q61) &&& (q40 === (!! true)); (q40 === (!! false)) &&& (q60 =/= q61)]);
          (q51 === (pour ())) &&&
            ((let b' = anotherBottle (fun q83 -> b === q83) in
              fresh (q79 q76 q66 q67) (q67 === (o ())) (lvl1 q66) (conde [(q66 === q67) &&& (q76 === (!! true)); (q76 === (!! false)) &&& (q66 =/= q67)])
                (conde
                   [(q76 === (!! true)) &&& (q79 === (!! true));
                   fresh (q71 q72) (q76 === (!! false)) (lvl2 q71) (q38 b' q72) (conde [(q71 === q72) &&& (q79 === (!! true)); (q79 === (!! false)) &&& (q71 =/= q72)])])
                (conde [(q79 === (!! true)) &&& (q40 === (!! false)); (q79 === (!! false)) &&& (q40 === (!! true))])))]))
let doStep q86 q88 q87 q89 =
  fresh (q90 f s q94 t b) (q90 === (pair f s)) (q94 === (pair t b)) (
    q86 q90) (q88 q94)
    (let lvl2 q95 = fresh (q96) (b === q96) (((q96 === (fst_ ())) &&& (s === q95)) ||| ((q96 === (snd_ ())) &&& (f === q95))) in
     fresh (q98) (t === q98)
       (conde
          [(q98 === (fill ())) &&& (createState (fun q105 -> b === q105) (q87 (fun q105 -> b === q105)) lvl2 q89);
          (q98 === (empty ())) &&& (createState (fun q105 -> b === q105) (fun q99 -> q99 === (o ())) lvl2 q89);
          (q98 === (pour ())) &&&
            ((let sum = add (fun q106 -> f === q106) (fun q107 -> s === q107) in
              let cap2 = q87 (anotherBottle (fun q105 -> b === q105)) in
              fresh (q100) (greater sum cap2 q100)
                (conde
                   [(q100 === (!! true)) &&& (createState (fun q105 -> b === q105) (sub sum cap2) cap2 q89);
                   (q100 === (!! false)) &&& (createState (fun q105 -> b === q105) (fun q102 -> q102 === (o ())) sum q89)])))]))
let isFinishState q108 q109 q110 =
  fresh (q111 f s q127 q124 q114 q115) (q111 === (pair f s)) (f === q114) (
    q115 === q127) (q108 q111) (q109 q127) (conde [(q114 === q115) &&& (q124 === (!! true)); (q124 === (!! false)) &&& (q114 =/= q115)])
    (conde
       [(q124 === (!! true)) &&& (q110 === (!! true));
       fresh (q119 q120) (q124 === (!! false)) (s === q119) (q120 === q127) (conde [(q119 === q120) &&& (q110 === (!! true)); (q110 === (!! false)) &&& (q119 =/= q120)])])
let checkAnswer answer capacities reqLvl q144 =
  let rec checkAnswer state0 q131 q140 =
    fresh (q141 q134) (state0 q141) (q131 q134)
      (((q134 === (nil ())) &&& (isFinishState (fun q142 -> q142 === q141) reqLvl q140)) |||
         (fresh (x xs q135) (q134 === (x % xs)) (checkStep (fun q142 -> q142 === q141) (fun q138 -> x === q138) capacities q135)
            (conde
               [(q135 === (!! true)) &&& (checkAnswer (doStep (fun q142 -> q142 === q141) (fun q138 -> x === q138) capacities) (fun q139 -> xs === q139) q140);
               (q135 === (!! false)) &&& (q140 === (!! false))]))) in
  let startState q143 = q143 === (pair (o ()) (o ())) in checkAnswer startState answer q144
let capacities1 q145 q146 =
  fresh (q147) (q145 q147) (((q147 === (fst_ ())) &&& (q146 === (s (s (s (s (o ()))))))) ||| ((q147 === (snd_ ())) &&& (q146 === (s (s (s (s (s (s (s (s (s (o ())))))))))))))