open GT
open OCanren
open OCanren.Std
type 'a0 gpeano =
  | O 
  | S of 'a0 
module For_gpeano = (Fmap)(struct let rec fmap fa0 = function | O -> O | S a0 -> S (fa0 a0)
                                  type 'a0 t = 'a0 gpeano end)
let rec o () = inj (For_gpeano.distrib O)
and s x__0 = inj (For_gpeano.distrib (S x__0))
type gperson =
  | A 
  | B 
  | C 
  | D 
let a () = !! A
let b () = !! B
let c () = !! C
let d () = !! D
type 'a0 gstep =
  | One of 'a0 
  | Two of 'a0 * 'a0 
module For_gstep = (Fmap)(struct let rec fmap fa0 = function | One a0 -> One (fa0 a0) | Two (a0_0, a0_1) -> Two ((fa0 a0_0), (fa0 a0_1))
                                 type 'a0 t = 'a0 gstep end)
let rec one x__0 = inj (For_gstep.distrib (One x__0))
and two x__0 x__1 = inj (For_gstep.distrib (Two (x__0, x__1)))
let rec greater_o a0_o b0_o q8 =
  fresh (q1) (a0_o q1)
    (((q1 === (o ())) &&& (q8 === (!! false))) |||
       (fresh (x q4) (q1 === (s x)) (b0_o q4) (((q4 === (o ())) &&& (q8 === (!! true))) ||| (fresh (y) (q4 === (s y)) (greater_o (fun q7 -> x === q7) (fun q6 -> y === q6) q8)))))
let grForPerson_o x_o y_o q30 =
  fresh (q10) (x_o q10)
    (conde
       [fresh (q12) (q10 === (a ())) (y_o q12)
          (conde
             [(q12 === (a ())) &&& (q30 === (!! false));
             (q12 === (b ())) &&& (q30 === (!! true));
             (q12 === (c ())) &&& (q30 === (!! true));
             (q12 === (d ())) &&& (q30 === (!! true))]);
       fresh (q18) (q10 === (b ())) (y_o q18)
         (conde
            [(q18 === (a ())) &&& (q30 === (!! false));
            (q18 === (b ())) &&& (q30 === (!! false));
            (q18 === (c ())) &&& (q30 === (!! false));
            (q18 === (d ())) &&& (q30 === (!! true))]);
       fresh (q24) (q10 === (c ())) (y_o q24)
         (conde
            [(q24 === (a ())) &&& (q30 === (!! false));
            (q24 === (b ())) &&& (q30 === (!! false));
            (q24 === (c ())) &&& (q30 === (!! false));
            (q24 === (d ())) &&& (q30 === (!! true))]);
       (q10 === (d ())) &&& (q30 === (!! false))])
let max_o a0_o b0_o q33 =
  fresh (q34 q35 q31) (a0_o q34) (b0_o q35) (greater_o (fun q36 -> q36 === q34) (fun q37 -> q37 === q35) q31)
    (conde [(q31 === (!! true)) &&& (q33 === q34); (q31 === (!! false)) &&& (q33 === q35)])
let rec add_o a0_o b0_o q43 =
  fresh (q39) (a0_o q39)
    (((q39 === (o ())) &&& (b0_o q43)) ||| (fresh (x) (q39 === (s x)) (add_o (fun q42 -> x === q42) (fun q41 -> fresh (q40) (q41 === (s q40)) (b0_o q40)) q43)))
let checkPerson_o q44 q45 q46 =
  fresh (q47 l a0 b0 c0 d0 q50) (q47 === (pair l (pair a0 (pair b0 (pair c0 d0))))) (
    q44 q47) (q45 q50)
    (conde
       [fresh (q53 q54) (q50 === (a ())) (a0 === q53) (l === q54) (conde [(q53 === q54) &&& (q46 === (!! true)); (q46 === (!! false)) &&& (q53 =/= q54)]);
       fresh (q58 q59) (q50 === (b ())) (b0 === q58) (l === q59) (conde [(q58 === q59) &&& (q46 === (!! true)); (q46 === (!! false)) &&& (q58 =/= q59)]);
       fresh (q63 q64) (q50 === (c ())) (c0 === q63) (l === q64) (conde [(q63 === q64) &&& (q46 === (!! true)); (q46 === (!! false)) &&& (q63 =/= q64)]);
       fresh (q68 q69) (q50 === (d ())) (d0 === q68) (l === q69) (conde [(q68 === q69) &&& (q46 === (!! true)); (q46 === (!! false)) &&& (q68 =/= q69)])])
let checkStep_o state_o q76 q91 =
  fresh (q92 q79) (state_o q92) (q76 q79)
    ((fresh (p) (q79 === (one p)) (checkPerson_o (fun q93 -> q93 === q92) (fun q80 -> p === q80) q91)) |||
       (fresh (p q q87) (q79 === (two p q)) (checkPerson_o (fun q93 -> q93 === q92) (fun q89 -> p === q89) q87)
          (conde
             [(q87 === (!! false)) &&& (q91 === (!! false));
             fresh (q83) (q87 === (!! true)) (checkPerson_o (fun q93 -> q93 === q92) (fun q90 -> q === q90) q83)
               (conde [(q83 === (!! false)) &&& (q91 === (!! false)); (q83 === (!! true)) &&& (grForPerson_o (fun q89 -> p === q89) (fun q90 -> q === q90) q91)])])))
let moveLight_o q94 q95 =
  fresh (q96 l a0 b0 c0 d0 q97 q98 q99 q100 q101 q104) (q96 === (pair l (pair a0 (pair b0 (pair c0 d0))))) (
    q95 === (pair q97 (pair q98 (pair q99 (pair q100 q101))))) (l === q104) (
    a0 === q98) (b0 === q99) (c0 === q100) (d0 === q101) (q94 q96) (conde [(q104 === (!! true)) &&& (q97 === (!! false)); (q104 === (!! false)) &&& (q97 === (!! true))])
let movePerson_o q111 q112 q113 =
  fresh (q114 l a0 b0 c0 d0 q117) (q114 === (pair l (pair a0 (pair b0 (pair c0 d0))))) (
    q111 q114) (q112 q117)
    (conde
       [fresh (q118 q119 q120 q121 q122 q125) (q117 === (a ())) (q113 === (pair q118 (pair q119 (pair q120 (pair q121 q122))))) (
          l === q118) (a0 === q125) (b0 === q120) (c0 === q121) (d0 === q122)
          (conde [(q125 === (!! true)) &&& (q119 === (!! false)); (q125 === (!! false)) &&& (q119 === (!! true))]);
       fresh (q127 q128 q129 q130 q131 q134) (q117 === (b ())) (q113 === (pair q127 (pair q128 (pair q129 (pair q130 q131))))) (
         l === q127) (a0 === q128) (b0 === q134) (c0 === q130) (d0 === q131)
         (conde [(q134 === (!! true)) &&& (q129 === (!! false)); (q134 === (!! false)) &&& (q129 === (!! true))]);
       fresh (q136 q137 q138 q139 q140 q143) (q117 === (c ())) (q113 === (pair q136 (pair q137 (pair q138 (pair q139 q140))))) (
         l === q136) (a0 === q137) (b0 === q138) (c0 === q143) (d0 === q140)
         (conde [(q143 === (!! true)) &&& (q139 === (!! false)); (q143 === (!! false)) &&& (q139 === (!! true))]);
       fresh (q145 q146 q147 q148 q149 q152) (q117 === (d ())) (q113 === (pair q145 (pair q146 (pair q147 (pair q148 q149))))) (
         l === q145) (a0 === q146) (b0 === q147) (c0 === q148) (d0 === q152)
         (conde [(q152 === (!! true)) &&& (q149 === (!! false)); (q152 === (!! false)) &&& (q149 === (!! true))])])
let step_o state_o q159 q166 =
  fresh (q162) (q159 q162)
    ((fresh (p) (q162 === (one p)) (moveLight_o (movePerson_o state_o (fun q163 -> p === q163)) q166)) |||
       (fresh (p q) (q162 === (two p q)) (moveLight_o (movePerson_o (movePerson_o state_o (fun q164 -> p === q164)) (fun q165 -> q === q165)) q166)))
let getTime_o state_o times_o q172 =
  fresh (q168) (state_o q168)
    ((fresh (p) (q168 === (one p)) (times_o (fun q169 -> p === q169) q172)) |||
       (fresh (p q) (q168 === (two p q)) (max_o (times_o (fun q170 -> p === q170)) (times_o (fun q171 -> q === q171)) q172)))
let getAnswer_o answer_o times_o q200 =
  let start_o q173 = q173 === (pair (!! true) (pair (!! true) (pair (!! true) (pair (!! true) (!! true))))) in
  let finish_o q174 = q174 === (pair (!! false) (pair (!! false) (pair (!! false) (pair (!! false) (!! false))))) in
  let rec getAnswer_o answer_o state_o q197 =
    fresh (q198 q176) (state_o q198) (answer_o q176)
      ((fresh (x xs q177) (q176 === (x % xs)) (checkStep_o (fun q199 -> q199 === q198) (fun q186 -> x === q186) q177)
          (conde
             [fresh (q181) (q177 === (!! true))
                (((q181 === (none ())) &&& (q197 === (none ()))) |||
                   (fresh (t1 q183) (q181 === (some t1)) (q197 === (some q183)) (add_o (getTime_o (fun q186 -> x === q186) times_o) (fun q185 -> t1 === q185) q183)))
                (getAnswer_o (fun q187 -> xs === q187) (step_o (fun q199 -> q199 === q198) (fun q186 -> x === q186)) q181);
             (q177 === (!! false)) &&& (q197 === (none ()))]))
         |||
         (fresh (q188 q194 q195) (q176 === (nil ())) (q194 === q198) (
            finish_o q195) (conde [(q194 === q195) &&& (q188 === (!! true)); (q188 === (!! false)) &&& (q194 =/= q195)])
            (conde [(q188 === (!! true)) &&& (q197 === (some (o ()))); (q188 === (!! false)) &&& (q197 === (none ()))]))) in
  getAnswer_o answer_o start_o q200
let standartTimes_o q201 q202 =
  fresh (q203) (q201 q203)
    (conde
       [(q203 === (a ())) &&& (q202 === (s (o ())));
       (q203 === (b ())) &&& (q202 === (s (s (o ()))));
       (q203 === (c ())) &&& (q202 === (s (s (s (s (s (o ())))))));
       (q203 === (d ())) &&& (q202 === (s (s (s (s (s (s (s (s (s (s (o ()))))))))))))])