open GT
open OCanren
open OCanren.Std
type gbottle =
  | Fst 
  | Snd 
let fst_ () = !! Fst
let snd_ () = !! Snd
type gstepType =
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
let rec add_o a_o b_o q5 =
  fresh (q1) (a_o q1) (((q1 === (o ())) &&& (b_o q5)) ||| (fresh (x) (q1 === (s x)) (add_o (fun q4 -> x === q4) (fun q3 -> fresh (q2) (q3 === (s q2)) (b_o q2)) q5)))
let rec greater_o a_o b_o q14 =
  fresh (q7) (a_o q7)
    (((q7 === (o ())) &&& (q14 === (!! false))) |||
       (fresh (x q10) (q7 === (s x)) (b_o q10)
          (((q10 === (o ())) &&& (q14 === (!! true))) ||| (fresh (y) (q10 === (s y)) (greater_o (fun q13 -> x === q13) (fun q12 -> y === q12) q14)))))
let rec sub_o a_o b_o q22 =
  fresh (q16) (b_o q16)
    (((q16 === (o ())) &&& (a_o q22)) |||
       (fresh (y q18) (q16 === (s y)) (a_o q18) (((q18 === (o ())) &&& (q22 === (o ()))) ||| (fresh (x) (q18 === (s x)) (sub_o (fun q20 -> x === q20) (fun q21 -> y === q21) q22)))))
let anotherBottle_o q23 q24 = fresh (q25) (q23 q25) (((q25 === (fst_ ())) &&& (q24 === (snd_ ()))) ||| ((q25 === (snd_ ())) &&& (q24 === (fst_ ()))))
let createState_o bottle_o lvl1_o lvl2_o q36 =
  fresh (q29) (bottle_o q29)
    ((fresh (q30 q31) (q29 === (fst_ ())) (q36 === (pair q30 q31)) (lvl1_o q30) (lvl2_o q31)) |||
       (fresh (q33 q34) (q29 === (snd_ ())) (q36 === (pair q33 q34)) (lvl2_o q33) (lvl1_o q34)))
let checkStep_o q37 q39 q38 q40 =
  fresh (q41 f s q45 t b) (q41 === (pair f s)) (q45 === (pair t b)) (
    q37 q41) (q39 q45)
    (let lvl1_o q46 = fresh (q47) (b === q47) (((q47 === (fst_ ())) &&& (f === q46)) ||| ((q47 === (snd_ ())) &&& (s === q46))) in
     let lvl2_o q48 = fresh (q49) (b === q49) (((q49 === (fst_ ())) &&& (s === q48)) ||| ((q49 === (snd_ ())) &&& (f === q48))) in
     fresh (q51) (t === q51)
       (conde
          [fresh (q55 q56) (q51 === (fill ())) (q56 === (o ())) (lvl1_o q55) (conde [(q55 === q56) &&& (q40 === (!! true)); (q40 === (!! false)) &&& (q55 =/= q56)]);
          fresh (q60 q61) (q51 === (empty ())) (lvl1_o q60) (q38 (fun q83 -> b === q83) q61)
            (conde [(q60 === q61) &&& (q40 === (!! true)); (q40 === (!! false)) &&& (q60 =/= q61)]);
          (q51 === (pour ())) &&&
            ((let b'_o = anotherBottle_o (fun q83 -> b === q83) in
              fresh (q79 q76 q66 q67) (q67 === (o ())) (lvl1_o q66) (
                conde [(q66 === q67) &&& (q76 === (!! true)); (q76 === (!! false)) &&& (q66 =/= q67)])
                (conde
                   [(q76 === (!! true)) &&& (q79 === (!! true));
                   fresh (q71 q72) (q76 === (!! false)) (lvl2_o q71) (q38 b'_o q72) (conde [(q71 === q72) &&& (q79 === (!! true)); (q79 === (!! false)) &&& (q71 =/= q72)])])
                (conde [(q79 === (!! true)) &&& (q40 === (!! false)); (q79 === (!! false)) &&& (q40 === (!! true))])))]))
let doStep_o q86 q88 q87 q89 =
  fresh (q90 f s q94 t b) (q90 === (pair f s)) (q94 === (pair t b)) (
    q86 q90) (q88 q94)
    (let lvl2_o q95 = fresh (q96) (b === q96) (((q96 === (fst_ ())) &&& (s === q95)) ||| ((q96 === (snd_ ())) &&& (f === q95))) in
     fresh (q98) (t === q98)
       (conde
          [(q98 === (fill ())) &&& (createState_o (fun q105 -> b === q105) (q87 (fun q105 -> b === q105)) lvl2_o q89);
          (q98 === (empty ())) &&& (createState_o (fun q105 -> b === q105) (fun q99 -> q99 === (o ())) lvl2_o q89);
          (q98 === (pour ())) &&&
            ((let sum_o = add_o (fun q106 -> f === q106) (fun q107 -> s === q107) in
              let cap2_o = q87 (anotherBottle_o (fun q105 -> b === q105)) in
              fresh (q100) (greater_o sum_o cap2_o q100)
                (conde
                   [(q100 === (!! true)) &&& (createState_o (fun q105 -> b === q105) (sub_o sum_o cap2_o) cap2_o q89);
                   (q100 === (!! false)) &&& (createState_o (fun q105 -> b === q105) (fun q102 -> q102 === (o ())) sum_o q89)])))]))
let isFinishState_o q108 q109 q110 =
  fresh (q111 f s q127 q124 q114 q115) (q111 === (pair f s)) (f === q114) (
    q115 === q127) (q108 q111) (q109 q127) (conde [(q114 === q115) &&& (q124 === (!! true)); (q124 === (!! false)) &&& (q114 =/= q115)])
    (conde
       [(q124 === (!! true)) &&& (q110 === (!! true));
       fresh (q119 q120) (q124 === (!! false)) (s === q119) (q120 === q127) (conde [(q119 === q120) &&& (q110 === (!! true)); (q110 === (!! false)) &&& (q119 =/= q120)])])
let checkAnswer_o answer_o capacities_o reqLvl_o q144 =
  let rec checkAnswer_o state0_o q131 q140 =
    fresh (q141 q134) (state0_o q141) (q131 q134)
      (((q134 === (nil ())) &&& (isFinishState_o (fun q142 -> q142 === q141) reqLvl_o q140)) |||
         (fresh (x xs q135) (q134 === (x % xs)) (checkStep_o (fun q142 -> q142 === q141) (fun q138 -> x === q138) capacities_o q135)
            (conde
               [(q135 === (!! true)) &&& (checkAnswer_o (doStep_o (fun q142 -> q142 === q141) (fun q138 -> x === q138) capacities_o) (fun q139 -> xs === q139) q140);
               (q135 === (!! false)) &&& (q140 === (!! false))]))) in
  let startState_o q143 = q143 === (pair (o ()) (o ())) in checkAnswer_o startState_o answer_o q144
let capacities1_o q145 q146 =
  fresh (q147) (q145 q147) (((q147 === (fst_ ())) &&& (q146 === (s (s (s (s (o ()))))))) ||| ((q147 === (snd_ ())) &&& (q146 === (s (s (s (s (s (s (s (s (s (o ())))))))))))))