open GT
open OCanren
open OCanren.Std
type 'a0 gnat =
  | Z 
  | S of 'a0 
module For_gnat = (Fmap)(struct let rec fmap fa0 = function | Z -> Z | S a0 -> S (fa0 a0)
                                type 'a0 t = 'a0 gnat end)
let rec z () = inj (For_gnat.distrib Z)
and s x__0 = inj (For_gnat.distrib (S x__0))
type 'a0 gset = {
  pin1: 'a0 ;
  pin2: 'a0 ;
  pin3: 'a0 }
module For_gset = (Fmap)(struct let rec fmap fa0 { pin1; pin2; pin3 } = { pin1 = (fa0 pin1); pin2 = (fa0 pin2); pin3 = (fa0 pin3) }
                                type 'a0 t = 'a0 gset end)
let ctor_gset pin1 pin2 pin3 = inj (For_gset.distrib { pin1; pin2; pin3 })
type gpin =
  | A 
  | B 
  | C 
let a () = !! A
let b () = !! B
let c () = !! C
let rec less_o x_o q0 q9 =
  fresh (q3 y' q5) (q3 === (s y')) (q0 q3) (x_o q5)
    (((q5 === (z ())) &&& (q9 === (!! true))) ||| (fresh (x') (q5 === (s x')) (less_o (fun q7 -> x' === q7) (fun q8 -> y' === q8) q9)))
let extra_o q10 q11 =
  fresh (q12) (q10 q12)
    (conde
       [(q12 === (pair (a ()) (b ()))) &&& (q11 === (c ()));
       (q12 === (pair (b ()) (a ()))) &&& (q11 === (c ()));
       (q12 === (pair (a ()) (c ()))) &&& (q11 === (b ()));
       (q12 === (pair (c ()) (a ()))) &&& (q11 === (b ()));
       (q12 === (pair (b ()) (c ()))) &&& (q11 === (a ()));
       (q12 === (pair (c ()) (b ()))) &&& (q11 === (a ()))])
let select_o s_o q19 q32 =
  fresh (q22) (q19 q22)
    (conde
       [fresh (q24 q25) (q22 === (a ())) (s_o (ctor_gset q32 q24 q25));
       fresh (q27 q28) (q22 === (b ())) (s_o (ctor_gset q27 q32 q28));
       fresh (q30 q31) (q22 === (c ())) (s_o (ctor_gset q30 q31 q32))])
let permut_o move_o s_o q41 =
  fresh (q42 q43 q34 x y q36 q37 q38) (q34 === q42) (q34 === (pair x y)) (
    q41 === (ctor_gset q36 q37 q38)) (move_o q42) (s_o q43) (select_o (fun q45 -> q45 === q43) (fun q39 -> x === q39) q36)
    (select_o (fun q45 -> q45 === q43) (fun q40 -> y === q40) q37) (select_o (fun q45 -> q45 === q43) (extra_o (fun q44 -> q44 === q42)) q38)
let tumrep_o move_o s_o q77 =
  fresh (q47 x y z q49) (q47 === (ctor_gset x y z)) (s_o q47) (move_o q49)
    (conde
       [fresh (q51 q52 q53) (q49 === (pair (a ()) (b ()))) (q77 === (ctor_gset q51 q52 q53)) (x === q51) (y === q52) (z === q53);
       fresh (q55 q56 q57) (q49 === (pair (b ()) (a ()))) (q77 === (ctor_gset q55 q56 q57)) (y === q55) (x === q56) (z === q57);
       fresh (q59 q60 q61) (q49 === (pair (a ()) (c ()))) (q77 === (ctor_gset q59 q60 q61)) (x === q59) (z === q60) (y === q61);
       fresh (q63 q64 q65) (q49 === (pair (c ()) (a ()))) (q77 === (ctor_gset q63 q64 q65)) (y === q63) (z === q64) (x === q65);
       fresh (q67 q68 q69) (q49 === (pair (b ()) (c ()))) (q77 === (ctor_gset q67 q68 q69)) (z === q67) (x === q68) (y === q69);
       fresh (q71 q72 q73) (q49 === (pair (c ()) (b ()))) (q77 === (ctor_gset q71 q72 q73)) (z === q71) (y === q72) (x === q73)])
let rec eval_o q131 q130 q132 =
  fresh (q134 q133) (q131 q134) (q130 q133)
    (Tabling.tabledrec (Tabling.succ (Tabling.succ Tabling.one))
       (fun q120 ->
          fun q129 ->
            fun q128 ->
              fun q119 ->
                fresh (q79) (q79 === q129)
                  (((q79 === (nil ())) &&& (q119 === q128)) |||
                     (fresh (move p' q81 x y q125 q124 q82 q112 q113) (
                        q79 === (move % p')) (move === q81) (q81 === (pair x y)) (
                        p' === q125) (x === q112) (y === q113) (conde [(q112 === q113) &&& (q82 === (!! true)); (q82 === (!! false)) &&& (q112 =/= q113)])
                        (conde
                           [(q82 === (!! true)) &&& (q124 === q128);
                           fresh (q85 topA restA onB onC) (q82 === (!! false)) (
                             q85 === (ctor_gset (topA % restA) onB onC)) (
                             permut_o (fun q117 -> move === q117) (fun q126 -> q126 === q128) q85)
                             (tumrep_o (fun q117 -> move === q117)
                                (fun q86 ->
                                   fresh (q87) (onB === q87)
                                     ((fresh (q89 q90 q91 q92) (q87 === (nil ())) (
                                         q86 === (ctor_gset q89 q90 q91)) (
                                         restA === q89) (q90 === (q92 % (nil ()))) (
                                         topA === q92) (onC === q91))
                                        |||
                                        (fresh (topB q94 q96 q98 q99 q100 q101 q102) (
                                           q87 === (topB % q94)) (q96 === (!! true)) (
                                           q86 === (ctor_gset q98 q99 q100)) (
                                           restA === q98) (q99 === (q101 % q102)) (
                                           topA === q101) (onB === q102) (
                                           onC === q100) (less_o (fun q106 -> topA === q106) (fun q104 -> topB === q104) q96)))) q124)]) (
                        q120 q125 q124 q119)))) q134 q133 q132)