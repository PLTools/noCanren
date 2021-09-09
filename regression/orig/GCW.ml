open GT
open OCanren
open OCanren.Std
type gperson =
  | G 
  | C 
  | W 
  | N 
let g () = !! G
let c () = !! C
let w () = !! W
let n () = !! N
let checkState_o q0 q1 =
  fresh (q2 i0 g0 c0 w0 q3 q21 q22) (q2 === (pair i0 (pair g0 (pair c0 w0)))) (
    i0 === q21) (g0 === q22) (q0 q2) (conde [(q21 === q22) &&& (q3 === (!! true)); (q3 === (!! false)) &&& (q21 =/= q22)])
    (conde
       [(q3 === (!! true)) &&& (q1 === (!! true));
       fresh (q5 q15 q16) (q3 === (!! false)) (i0 === q15) (c0 === q16) (
         conde [(q15 === q16) &&& (q5 === (!! true)); (q5 === (!! false)) &&& (q15 =/= q16)])
         (conde
            [fresh (q10 q11) (q5 === (!! true)) (i0 === q10) (w0 === q11) (conde [(q10 === q11) &&& (q1 === (!! true)); (q1 === (!! false)) &&& (q10 =/= q11)]);
            (q5 === (!! false)) &&& (q1 === (!! false))])])
let checkStep_o q28 q29 q30 =
  fresh (q31 i0 g0 c0 w0 q34) (q31 === (pair i0 (pair g0 (pair c0 w0)))) (
    q28 q31) (q29 q34)
    (conde
       [(q34 === (n ())) &&& (q30 === (!! true));
       fresh (q38 q39) (q34 === (g ())) (i0 === q38) (g0 === q39) (conde [(q38 === q39) &&& (q30 === (!! true)); (q30 === (!! false)) &&& (q38 =/= q39)]);
       fresh (q43 q44) (q34 === (c ())) (i0 === q43) (c0 === q44) (conde [(q43 === q44) &&& (q30 === (!! true)); (q30 === (!! false)) &&& (q43 =/= q44)]);
       fresh (q48 q49) (q34 === (w ())) (i0 === q48) (w0 === q49) (conde [(q48 === q49) &&& (q30 === (!! true)); (q30 === (!! false)) &&& (q48 =/= q49)])])
let step_o q55 q56 q57 =
  fresh (q58 i0 g0 c0 w0 q61) (q58 === (pair i0 (pair g0 (pair c0 w0)))) (
    q55 q58) (q56 q61)
    (conde
       [fresh (q62 q63 q64 q65 q68 q71) (q61 === (g ())) (q57 === (pair q62 (pair q63 (pair q64 q65)))) (
          i0 === q68) (g0 === q71) (c0 === q64) (w0 === q65) (conde [(q68 === (!! true)) &&& (q62 === (!! false)); (q68 === (!! false)) &&& (q62 === (!! true))])
          (conde [(q71 === (!! true)) &&& (q63 === (!! false)); (q71 === (!! false)) &&& (q63 === (!! true))]);
       fresh (q73 q74 q75 q76 q79 q82) (q61 === (c ())) (q57 === (pair q73 (pair q74 (pair q75 q76)))) (
         i0 === q79) (g0 === q74) (c0 === q82) (w0 === q76) (conde [(q79 === (!! true)) &&& (q73 === (!! false)); (q79 === (!! false)) &&& (q73 === (!! true))])
         (conde [(q82 === (!! true)) &&& (q75 === (!! false)); (q82 === (!! false)) &&& (q75 === (!! true))]);
       fresh (q84 q85 q86 q87 q90 q93) (q61 === (w ())) (q57 === (pair q84 (pair q85 (pair q86 q87)))) (
         i0 === q90) (g0 === q85) (c0 === q86) (w0 === q93) (conde [(q90 === (!! true)) &&& (q84 === (!! false)); (q90 === (!! false)) &&& (q84 === (!! true))])
         (conde [(q93 === (!! true)) &&& (q87 === (!! false)); (q93 === (!! false)) &&& (q87 === (!! true))]);
       fresh (q95 q96 q97 q98 q101) (q61 === (n ())) (q57 === (pair q95 (pair q96 (pair q97 q98)))) (
         i0 === q101) (g0 === q96) (c0 === q97) (w0 === q98) (conde [(q101 === (!! true)) &&& (q95 === (!! false)); (q101 === (!! false)) &&& (q95 === (!! true))])])
let checkAnswer_o a_o q127 =
  let startState_o q107 = q107 === (pair (!! true) (pair (!! true) (pair (!! true) (!! true)))) in
  let finishState_o q108 = q108 === (pair (!! false) (pair (!! false) (pair (!! false) (!! false)))) in
  let rec checkAnswer_o a_o state_o q124 =
    fresh (q125 q110) (state_o q125) (a_o q110)
      ((fresh (q113 q114) (q110 === (nil ())) (q113 === q125) (finishState_o q114) (conde [(q113 === q114) &&& (q124 === (!! true)); (q124 === (!! false)) &&& (q113 =/= q114)]))
         |||
         (fresh (x xs q116) (q110 === (x % xs)) (checkStep_o (fun q126 -> q126 === q125) (fun q122 -> x === q122) q116)
            (conde
               [(q116 === (!! true)) &&&
                  ((let newState_o = step_o (fun q126 -> q126 === q125) (fun q122 -> x === q122) in
                    fresh (q119) (checkState_o newState_o q119)
                      (conde [(q119 === (!! true)) &&& (checkAnswer_o (fun q123 -> xs === q123) newState_o q124); (q119 === (!! false)) &&& (q124 === (!! false))])));
               (q116 === (!! false)) &&& (q124 === (!! false))]))) in
  checkAnswer_o a_o startState_o q127