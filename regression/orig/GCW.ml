open GT
open OCanren
open OCanren.Std
type person =
  | G 
  | C 
  | W 
  | N 
let g () = !! G
let c () = !! C
let w () = !! W
let n () = !! N
type 'a0 gstate =
  | St of 'a0 * 'a0 * 'a0 * 'a0 
module For_gstate = (Fmap)(struct let rec fmap fa0 = function | St (a0_0, a0_1, a0_2, a0_3) -> St ((fa0 a0_0), (fa0 a0_1), (fa0 a0_2), (fa0 a0_3))
                                  type 'a0 t = 'a0 gstate end)
let rec st x__0 x__1 x__2 x__3 = inj (For_gstate.distrib (St (x__0, x__1, x__2, x__3)))
let checkState s q27 =
  fresh (q1 i0 g0 c0 w0 q2 q20 q21) (q1 === (st i0 g0 c0 w0)) (i0 === q20) (
    g0 === q21) (s q1) (conde [(q20 === q21) &&& (q2 === (!! true)); (q2 === (!! false)) &&& (q20 =/= q21)])
    (conde
       [(q2 === (!! true)) &&& (q27 === (!! true));
       fresh (q4 q14 q15) (q2 === (!! false)) (i0 === q14) (c0 === q15) (
         conde [(q14 === q15) &&& (q4 === (!! true)); (q4 === (!! false)) &&& (q14 =/= q15)])
         (conde
            [fresh (q9 q10) (q4 === (!! true)) (i0 === q9) (w0 === q10) (conde [(q9 === q10) &&& (q27 === (!! true)); (q27 === (!! false)) &&& (q9 =/= q10)]);
            (q4 === (!! false)) &&& (q27 === (!! false))])])
let checkStep state step q52 =
  fresh (q29 i0 g0 c0 w0 q31) (q29 === (st i0 g0 c0 w0)) (state q29) (
    step q31)
    (conde
       [(q31 === (n ())) &&& (q52 === (!! true));
       fresh (q35 q36) (q31 === (g ())) (i0 === q35) (g0 === q36) (conde [(q35 === q36) &&& (q52 === (!! true)); (q52 === (!! false)) &&& (q35 =/= q36)]);
       fresh (q40 q41) (q31 === (c ())) (i0 === q40) (c0 === q41) (conde [(q40 === q41) &&& (q52 === (!! true)); (q52 === (!! false)) &&& (q40 =/= q41)]);
       fresh (q45 q46) (q31 === (w ())) (i0 === q45) (w0 === q46) (conde [(q45 === q46) &&& (q52 === (!! true)); (q52 === (!! false)) &&& (q45 =/= q46)])])
let step s p q102 =
  fresh (q54 i0 g0 c0 w0 q56) (q54 === (st i0 g0 c0 w0)) (s q54) (p q56)
    (conde
       [fresh (q57 q58 q59 q60 q63 q66) (q56 === (g ())) (q102 === (st q57 q58 q59 q60)) (
          i0 === q63) (g0 === q66) (c0 === q59) (w0 === q60) (conde [(q63 === (!! true)) &&& (q57 === (!! false)); (q63 === (!! false)) &&& (q57 === (!! true))])
          (conde [(q66 === (!! true)) &&& (q58 === (!! false)); (q66 === (!! false)) &&& (q58 === (!! true))]);
       fresh (q68 q69 q70 q71 q74 q77) (q56 === (c ())) (q102 === (st q68 q69 q70 q71)) (
         i0 === q74) (g0 === q69) (c0 === q77) (w0 === q71) (conde [(q74 === (!! true)) &&& (q68 === (!! false)); (q74 === (!! false)) &&& (q68 === (!! true))])
         (conde [(q77 === (!! true)) &&& (q70 === (!! false)); (q77 === (!! false)) &&& (q70 === (!! true))]);
       fresh (q79 q80 q81 q82 q85 q88) (q56 === (w ())) (q102 === (st q79 q80 q81 q82)) (
         i0 === q85) (g0 === q80) (c0 === q81) (w0 === q88) (conde [(q85 === (!! true)) &&& (q79 === (!! false)); (q85 === (!! false)) &&& (q79 === (!! true))])
         (conde [(q88 === (!! true)) &&& (q82 === (!! false)); (q88 === (!! false)) &&& (q82 === (!! true))]);
       fresh (q90 q91 q92 q93 q96) (q56 === (n ())) (q102 === (st q90 q91 q92 q93)) (
         i0 === q96) (g0 === q91) (c0 === q92) (w0 === q93) (conde [(q96 === (!! true)) &&& (q90 === (!! false)); (q96 === (!! false)) &&& (q90 === (!! true))])])
let checkAnswer a q123 =
  let startState q103 = q103 === (st (!! true) (!! true) (!! true) (!! true)) in
  let finishState q104 = q104 === (st (!! false) (!! false) (!! false) (!! false)) in
  let rec checkAnswer a state q120 =
    fresh (q121 q106) (state q121) (a q106)
      ((fresh (q109 q110) (q106 === (nil ())) (q109 === q121) (finishState q110) (conde [(q109 === q110) &&& (q120 === (!! true)); (q120 === (!! false)) &&& (q109 =/= q110)])) |||
         (fresh (x xs q112) (q106 === (x % xs)) (checkStep (fun q122 -> q122 === q121) (fun q118 -> x === q118) q112)
            (conde
               [(q112 === (!! true)) &&&
                  ((let newState = step (fun q122 -> q122 === q121) (fun q118 -> x === q118) in
                    fresh (q115) (checkState newState q115)
                      (conde [(q115 === (!! true)) &&& (checkAnswer (fun q119 -> xs === q119) newState q120); (q115 === (!! false)) &&& (q120 === (!! false))])));
               (q112 === (!! false)) &&& (q120 === (!! false))]))) in
  checkAnswer a startState q123