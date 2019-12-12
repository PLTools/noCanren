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
type person =
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
type 'a0 gstate =
  | St of 'a0 * 'a0 * 'a0 * 'a0 * 'a0 
module For_gstate =
  (Fmap)(struct let rec fmap fa0 = function | St (a0_0, a0_1, a0_2, a0_3, a0_4) -> St ((fa0 a0_0), (fa0 a0_1), (fa0 a0_2), (fa0 a0_3), (fa0 a0_4))
                type 'a0 t = 'a0 gstate end)
let rec st x__0 x__1 x__2 x__3 x__4 = inj (For_gstate.distrib (St (x__0, x__1, x__2, x__3, x__4)))
let rec greater a0 b0 q8 =
  fresh (q1) (a0 q1)
    (((q1 === (o ())) &&& (q8 === (!! false))) |||
       (fresh (x q4) (q1 === (s x)) (b0 q4) (((q4 === (o ())) &&& (q8 === (!! true))) ||| (fresh (y) (q4 === (s y)) (greater (fun q7 -> x === q7) (fun q6 -> y === q6) q8)))))
let grForPerson x y q30 =
  fresh (q10) (x q10)
    (conde
       [fresh (q12) (q10 === (a ())) (y q12)
          (conde
             [(q12 === (a ())) &&& (q30 === (!! false));
             (q12 === (b ())) &&& (q30 === (!! true));
             (q12 === (c ())) &&& (q30 === (!! true));
             (q12 === (d ())) &&& (q30 === (!! true))]);
       fresh (q18) (q10 === (b ())) (y q18)
         (conde
            [(q18 === (a ())) &&& (q30 === (!! false));
            (q18 === (b ())) &&& (q30 === (!! false));
            (q18 === (c ())) &&& (q30 === (!! false));
            (q18 === (d ())) &&& (q30 === (!! true))]);
       fresh (q24) (q10 === (c ())) (y q24)
         (conde
            [(q24 === (a ())) &&& (q30 === (!! false));
            (q24 === (b ())) &&& (q30 === (!! false));
            (q24 === (c ())) &&& (q30 === (!! false));
            (q24 === (d ())) &&& (q30 === (!! true))]);
       (q10 === (d ())) &&& (q30 === (!! false))])
let max a0 b0 q33 =
  fresh (q34 q35 q31) (a0 q34) (b0 q35) (greater (fun q36 -> q36 === q34) (fun q37 -> q37 === q35) q31)
    (conde [(q31 === (!! true)) &&& (q33 === q34); (q31 === (!! false)) &&& (q33 === q35)])
let rec add a0 b0 q43 =
  fresh (q39) (a0 q39) (((q39 === (o ())) &&& (b0 q43)) ||| (fresh (x) (q39 === (s x)) (add (fun q42 -> x === q42) (fun q41 -> fresh (q40) (q41 === (s q40)) (b0 q40)) q43)))
let checkPerson state person q73 =
  fresh (q45 l a0 b0 c0 d0 q47) (q45 === (st l a0 b0 c0 d0)) (state q45) (
    person q47)
    (conde
       [fresh (q50 q51) (q47 === (a ())) (a0 === q50) (l === q51) (conde [(q50 === q51) &&& (q73 === (!! true)); (q73 === (!! false)) &&& (q50 =/= q51)]);
       fresh (q55 q56) (q47 === (b ())) (b0 === q55) (l === q56) (conde [(q55 === q56) &&& (q73 === (!! true)); (q73 === (!! false)) &&& (q55 =/= q56)]);
       fresh (q60 q61) (q47 === (c ())) (c0 === q60) (l === q61) (conde [(q60 === q61) &&& (q73 === (!! true)); (q73 === (!! false)) &&& (q60 =/= q61)]);
       fresh (q65 q66) (q47 === (d ())) (d0 === q65) (l === q66) (conde [(q65 === q66) &&& (q73 === (!! true)); (q73 === (!! false)) &&& (q65 =/= q66)])])
let checkStep state step q87 =
  fresh (q88 q75) (state q88) (step q75)
    ((fresh (p) (q75 === (one p)) (checkPerson (fun q89 -> q89 === q88) (fun q76 -> p === q76) q87)) |||
       (fresh (p q q83) (q75 === (two p q)) (checkPerson (fun q89 -> q89 === q88) (fun q85 -> p === q85) q83)
          (conde
             [(q83 === (!! false)) &&& (q87 === (!! false));
             fresh (q79) (q83 === (!! true)) (checkPerson (fun q89 -> q89 === q88) (fun q86 -> q === q86) q79)
               (conde [(q79 === (!! false)) &&& (q87 === (!! false)); (q79 === (!! true)) &&& (grForPerson (fun q85 -> p === q85) (fun q86 -> q === q86) q87)])])))
let moveLight state q106 =
  fresh (q91 l a0 b0 c0 d0 q92 q93 q94 q95 q96 q99) (q91 === (st l a0 b0 c0 d0)) (
    q106 === (st q92 q93 q94 q95 q96)) (l === q99) (a0 === q93) (b0 === q94) (
    c0 === q95) (d0 === q96) (state q91) (conde [(q99 === (!! true)) &&& (q92 === (!! false)); (q99 === (!! false)) &&& (q92 === (!! true))])
let movePerson state person q152 =
  fresh (q108 l a0 b0 c0 d0 q110) (q108 === (st l a0 b0 c0 d0)) (state q108) (
    person q110)
    (conde
       [fresh (q111 q112 q113 q114 q115 q118) (q110 === (a ())) (q152 === (st q111 q112 q113 q114 q115)) (
          l === q111) (a0 === q118) (b0 === q113) (c0 === q114) (d0 === q115)
          (conde [(q118 === (!! true)) &&& (q112 === (!! false)); (q118 === (!! false)) &&& (q112 === (!! true))]);
       fresh (q120 q121 q122 q123 q124 q127) (q110 === (b ())) (q152 === (st q120 q121 q122 q123 q124)) (
         l === q120) (a0 === q121) (b0 === q127) (c0 === q123) (d0 === q124)
         (conde [(q127 === (!! true)) &&& (q122 === (!! false)); (q127 === (!! false)) &&& (q122 === (!! true))]);
       fresh (q129 q130 q131 q132 q133 q136) (q110 === (c ())) (q152 === (st q129 q130 q131 q132 q133)) (
         l === q129) (a0 === q130) (b0 === q131) (c0 === q136) (d0 === q133)
         (conde [(q136 === (!! true)) &&& (q132 === (!! false)); (q136 === (!! false)) &&& (q132 === (!! true))]);
       fresh (q138 q139 q140 q141 q142 q145) (q110 === (d ())) (q152 === (st q138 q139 q140 q141 q142)) (
         l === q138) (a0 === q139) (b0 === q140) (c0 === q141) (d0 === q145)
         (conde [(q145 === (!! true)) &&& (q142 === (!! false)); (q145 === (!! false)) &&& (q142 === (!! true))])])
let step state step q158 =
  fresh (q154) (step q154)
    ((fresh (p) (q154 === (one p)) (moveLight (movePerson state (fun q155 -> p === q155)) q158)) |||
       (fresh (p q) (q154 === (two p q)) (moveLight (movePerson (movePerson state (fun q156 -> p === q156)) (fun q157 -> q === q157)) q158)))
let getTime state times q164 =
  fresh (q160) (state q160)
    ((fresh (p) (q160 === (one p)) (times (fun q161 -> p === q161) q164)) |||
       (fresh (p q) (q160 === (two p q)) (max (times (fun q162 -> p === q162)) (times (fun q163 -> q === q163)) q164)))
let getAnswer answer times q192 =
  let start q165 = q165 === (st (!! true) (!! true) (!! true) (!! true) (!! true)) in
  let finish q166 = q166 === (st (!! false) (!! false) (!! false) (!! false) (!! false)) in
  let rec getAnswer answer state q189 =
    fresh (q190 q168) (state q190) (answer q168)
      ((fresh (x xs q169) (q168 === (x % xs)) (checkStep (fun q191 -> q191 === q190) (fun q178 -> x === q178) q169)
          (conde
             [fresh (q173) (q169 === (!! true))
                (((q173 === (none ())) &&& (q189 === (none ()))) |||
                   (fresh (t1 q175) (q173 === (some t1)) (q189 === (some q175)) (add (getTime (fun q178 -> x === q178) times) (fun q177 -> t1 === q177) q175)))
                (getAnswer (fun q179 -> xs === q179) (step (fun q191 -> q191 === q190) (fun q178 -> x === q178)) q173);
             (q169 === (!! false)) &&& (q189 === (none ()))]))
         |||
         (fresh (q180 q186 q187) (q168 === (nil ())) (q186 === q190) (
            finish q187) (conde [(q186 === q187) &&& (q180 === (!! true)); (q180 === (!! false)) &&& (q186 =/= q187)])
            (conde [(q180 === (!! true)) &&& (q189 === (some (o ()))); (q180 === (!! false)) &&& (q189 === (none ()))]))) in
  getAnswer answer start q192
let standartTimes p q199 =
  fresh (q194) (p q194)
    (conde
       [(q194 === (a ())) &&& (q199 === (s (o ())));
       (q194 === (b ())) &&& (q199 === (s (s (o ()))));
       (q194 === (c ())) &&& (q199 === (s (s (s (s (s (o ())))))));
       (q194 === (d ())) &&& (q199 === (s (s (s (s (s (s (s (s (s (s (o ()))))))))))))])