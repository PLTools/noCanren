open GT
open OCanren
open OCanren.Std
type hause_color =
  | Yellow 
  | Blue 
  | Red 
  | Ivory 
  | Green 
let yellow () = !! Yellow
let blue () = !! Blue
let red () = !! Red
let ivory () = !! Ivory
let green () = !! Green
type nationality =
  | Norwegian 
  | Ukrainian 
  | Englishman 
  | Spaniard 
  | Japanese 
let norwegian () = !! Norwegian
let ukrainian () = !! Ukrainian
let englishman () = !! Englishman
let spaniard () = !! Spaniard
let japanese () = !! Japanese
type drink =
  | Water 
  | Tea 
  | Milk 
  | Orange_juice 
  | Coffee 
let water () = !! Water
let tea () = !! Tea
let milk () = !! Milk
let orange_juice () = !! Orange_juice
let coffee () = !! Coffee
type smoke =
  | Kools 
  | Chesterfield 
  | Old_Gold 
  | Lacky_Strike 
  | Parliament 
let kools () = !! Kools
let chesterfield () = !! Chesterfield
let old_Gold () = !! Old_Gold
let lacky_Strike () = !! Lacky_Strike
let parliament () = !! Parliament
type pet =
  | Fox 
  | Hourse 
  | Snails 
  | Dog 
  | Zebra 
let fox () = !! Fox
let hourse () = !! Hourse
let snails () = !! Snails
let dog () = !! Dog
let zebra () = !! Zebra
type ('a4, 'a3, 'a2, 'a1, 'a0) gperson =
  | Person of 'a4 * 'a3 * 'a2 * 'a1 * 'a0 
module For_gperson =
  (Fmap5)(struct
            let rec fmap fa4 fa3 fa2 fa1 fa0 = function | Person (a4_0, a3_1, a2_2, a1_3, a0_4) -> Person ((fa4 a4_0), (fa3 a3_1), (fa2 a2_2), (fa1 a1_3), (fa0 a0_4))
            type ('a4, 'a3, 'a2, 'a1, 'a0) t = ('a4, 'a3, 'a2, 'a1, 'a0) gperson
          end)
let rec person x__0 x__1 x__2 x__3 x__4 = inj (For_gperson.distrib (Person (x__0, x__1, x__2, x__3, x__4)))
type 'a0 gstate =
  | State of 'a0 * 'a0 * 'a0 * 'a0 * 'a0 
module For_gstate =
  (Fmap)(struct let rec fmap fa0 = function | State (a0_0, a0_1, a0_2, a0_3, a0_4) -> State ((fa0 a0_0), (fa0 a0_1), (fa0 a0_2), (fa0 a0_3), (fa0 a0_4))
                type 'a0 t = 'a0 gstate end)
let rec state x__0 x__1 x__2 x__3 x__4 = inj (For_gstate.distrib (State (x__0, x__1, x__2, x__3, x__4)))
let all_different st q99 =
  let two_different a b q55 =
    fresh (q1 c1 n1 d1 s1 p1 q3 c2 n2 d2 s2 p2 q43 q6 q7) (q1 === (person c1 n1 d1 s1 p1)) (
      q3 === (person c2 n2 d2 s2 p2)) (c1 === q6) (c2 === q7) (a q1) (
      b q3) (conde [(q6 === q7) &&& (q43 === (!! false)); (q43 === (!! true)) &&& (q6 =/= q7)])
      (conde
         [(q43 === (!! false)) &&& (q55 === (!! false));
         fresh (q39 q11 q12) (q43 === (!! true)) (n1 === q11) (n2 === q12) (
           conde [(q11 === q12) &&& (q39 === (!! false)); (q39 === (!! true)) &&& (q11 =/= q12)])
           (conde
              [(q39 === (!! false)) &&& (q55 === (!! false));
              fresh (q35 q16 q17) (q39 === (!! true)) (d1 === q16) (d2 === q17) (
                conde [(q16 === q17) &&& (q35 === (!! false)); (q35 === (!! true)) &&& (q16 =/= q17)])
                (conde
                   [(q35 === (!! false)) &&& (q55 === (!! false));
                   fresh (q31 q21 q22) (q35 === (!! true)) (s1 === q21) (
                     s2 === q22) (conde [(q21 === q22) &&& (q31 === (!! false)); (q31 === (!! true)) &&& (q21 =/= q22)])
                     (conde
                        [(q31 === (!! false)) &&& (q55 === (!! false));
                        fresh (q26 q27) (q31 === (!! true)) (p1 === q26) (p2 === q27) (conde [(q26 === q27) &&& (q55 === (!! false)); (q55 === (!! true)) &&& (q26 =/= q27)])])])])]) in
  fresh (q57 p1 p2 p3 p4 p5 q92) (q57 === (state p1 p2 p3 p4 p5)) (st q57) (
    two_different (fun q94 -> p1 === q94) (fun q95 -> p2 === q95) q92)
    (conde
       [(q92 === (!! false)) &&& (q99 === (!! false));
       fresh (q88) (q92 === (!! true)) (two_different (fun q94 -> p1 === q94) (fun q96 -> p3 === q96) q88)
         (conde
            [(q88 === (!! false)) &&& (q99 === (!! false));
            fresh (q84) (q88 === (!! true)) (two_different (fun q94 -> p1 === q94) (fun q97 -> p4 === q97) q84)
              (conde
                 [(q84 === (!! false)) &&& (q99 === (!! false));
                 fresh (q80) (q84 === (!! true)) (two_different (fun q94 -> p1 === q94) (fun q98 -> p5 === q98) q80)
                   (conde
                      [(q80 === (!! false)) &&& (q99 === (!! false));
                      fresh (q76) (q80 === (!! true)) (two_different (fun q95 -> p2 === q95) (fun q96 -> p3 === q96) q76)
                        (conde
                           [(q76 === (!! false)) &&& (q99 === (!! false));
                           fresh (q72) (q76 === (!! true)) (two_different (fun q95 -> p2 === q95) (fun q97 -> p4 === q97) q72)
                             (conde
                                [(q72 === (!! false)) &&& (q99 === (!! false));
                                fresh (q68) (q72 === (!! true)) (two_different (fun q95 -> p2 === q95) (fun q98 -> p5 === q98) q68)
                                  (conde
                                     [(q68 === (!! false)) &&& (q99 === (!! false));
                                     fresh (q64) (q68 === (!! true)) (
                                       two_different (fun q96 -> p3 === q96) (fun q97 -> p4 === q97) q64)
                                       (conde
                                          [(q64 === (!! false)) &&& (q99 === (!! false));
                                          fresh (q60) (q64 === (!! true)) (
                                            two_different (fun q96 -> p3 === q96) (fun q98 -> p5 === q98) q60)
                                            (conde
                                               [(q60 === (!! false)) &&& (q99 === (!! false));
                                               (q60 === (!! true)) &&& (two_different (fun q97 -> p4 === q97) (fun q98 -> p5 === q98) q99)])])])])])])])])])
let any_of_person f st q123 =
  fresh (q101 p1 p2 p3 p4 p5 q116) (q101 === (state p1 p2 p3 p4 p5)) (
    st q101) (f (fun q118 -> p1 === q118) q116)
    (conde
       [(q116 === (!! true)) &&& (q123 === (!! true));
       fresh (q112) (q116 === (!! false)) (f (fun q119 -> p2 === q119) q112)
         (conde
            [(q112 === (!! true)) &&& (q123 === (!! true));
            fresh (q108) (q112 === (!! false)) (f (fun q120 -> p3 === q120) q108)
              (conde
                 [(q108 === (!! true)) &&& (q123 === (!! true));
                 fresh (q104) (q108 === (!! false)) (f (fun q121 -> p4 === q121) q104)
                   (conde [(q104 === (!! true)) &&& (q123 === (!! true)); (q104 === (!! false)) &&& (f (fun q122 -> p5 === q122) q123)])])])])
let any_of_neighbors_pair f st q143 =
  fresh (q125 p1 p2 p3 p4 p5 q136) (q125 === (state p1 p2 p3 p4 p5)) (
    st q125) (f (fun q138 -> p1 === q138) (fun q139 -> p2 === q139) q136)
    (conde
       [(q136 === (!! true)) &&& (q143 === (!! true));
       fresh (q132) (q136 === (!! false)) (f (fun q139 -> p2 === q139) (fun q140 -> p3 === q140) q132)
         (conde
            [(q132 === (!! true)) &&& (q143 === (!! true));
            fresh (q128) (q132 === (!! false)) (f (fun q140 -> p3 === q140) (fun q141 -> p4 === q141) q128)
              (conde [(q128 === (!! true)) &&& (q143 === (!! true)); (q128 === (!! false)) &&& (f (fun q141 -> p4 === q141) (fun q142 -> p5 === q142) q143)])])])
let clue02 st q171 =
  let for_person per q170 =
    fresh (q145 c n q146 q147 q148 q163 q152 q153) (q145 === (person c n q146 q147 q148)) (
      n === q152) (q153 === (englishman ())) (per q145) (conde [(q152 === q153) &&& (q163 === (!! true)); (q163 === (!! false)) &&& (q152 =/= q153)])
      (conde
         [(q163 === (!! false)) &&& (q170 === (!! false));
         fresh (q158 q159) (q163 === (!! true)) (c === q158) (q159 === (red ())) (conde [(q158 === q159) &&& (q170 === (!! true)); (q170 === (!! false)) &&& (q158 =/= q159)])]) in
  any_of_person for_person st q171
let clue03 st q199 =
  let for_person per q198 =
    fresh (q173 q174 n q175 q176 p q191 q180 q181) (q173 === (person q174 n q175 q176 p)) (
      n === q180) (q181 === (spaniard ())) (per q173) (conde [(q180 === q181) &&& (q191 === (!! true)); (q191 === (!! false)) &&& (q180 =/= q181)])
      (conde
         [(q191 === (!! false)) &&& (q198 === (!! false));
         fresh (q186 q187) (q191 === (!! true)) (p === q186) (q187 === (dog ())) (conde [(q186 === q187) &&& (q198 === (!! true)); (q198 === (!! false)) &&& (q186 =/= q187)])]) in
  any_of_person for_person st q199
let clue04 st q227 =
  let for_person per q226 =
    fresh (q201 c q202 d q203 q204 q219 q208 q209) (q201 === (person c q202 d q203 q204)) (
      c === q208) (q209 === (green ())) (per q201) (conde [(q208 === q209) &&& (q219 === (!! true)); (q219 === (!! false)) &&& (q208 =/= q209)])
      (conde
         [(q219 === (!! false)) &&& (q226 === (!! false));
         fresh (q214 q215) (q219 === (!! true)) (d === q214) (q215 === (coffee ())) (conde [(q214 === q215) &&& (q226 === (!! true)); (q226 === (!! false)) &&& (q214 =/= q215)])]) in
  any_of_person for_person st q227
let clue05 st q255 =
  let for_person per q254 =
    fresh (q229 q230 n d q231 q232 q247 q236 q237) (q229 === (person q230 n d q231 q232)) (
      n === q236) (q237 === (ukrainian ())) (per q229) (conde [(q236 === q237) &&& (q247 === (!! true)); (q247 === (!! false)) &&& (q236 =/= q237)])
      (conde
         [(q247 === (!! false)) &&& (q254 === (!! false));
         fresh (q242 q243) (q247 === (!! true)) (d === q242) (q243 === (tea ())) (conde [(q242 === q243) &&& (q254 === (!! true)); (q254 === (!! false)) &&& (q242 =/= q243)])]) in
  any_of_person for_person st q255
let clue06 st q295 =
  let for_neighbors_pair per1 per2 q294 =
    fresh (q257 c1 q258 q259 q260 q261 q263 c2 q264 q265 q266 q267 q282 q271 q272) (
      q257 === (person c1 q258 q259 q260 q261)) (q263 === (person c2 q264 q265 q266 q267)) (
      c1 === q271) (q272 === (ivory ())) (per1 q257) (per2 q263) (conde [(q271 === q272) &&& (q282 === (!! true)); (q282 === (!! false)) &&& (q271 =/= q272)])
      (conde
         [(q282 === (!! false)) &&& (q294 === (!! false));
         fresh (q277 q278) (q282 === (!! true)) (c2 === q277) (q278 === (green ())) (conde [(q277 === q278) &&& (q294 === (!! true)); (q294 === (!! false)) &&& (q277 =/= q278)])]) in
  any_of_neighbors_pair for_neighbors_pair st q295
let clue07 st q323 =
  let for_person per q322 =
    fresh (q297 q298 q299 q300 s p q315 q304 q305) (q297 === (person q298 q299 q300 s p)) (
      s === q304) (q305 === (old_Gold ())) (per q297) (conde [(q304 === q305) &&& (q315 === (!! true)); (q315 === (!! false)) &&& (q304 =/= q305)])
      (conde
         [(q315 === (!! false)) &&& (q322 === (!! false));
         fresh (q310 q311) (q315 === (!! true)) (p === q310) (q311 === (snails ())) (conde [(q310 === q311) &&& (q322 === (!! true)); (q322 === (!! false)) &&& (q310 =/= q311)])]) in
  any_of_person for_person st q323
let clue08 st q351 =
  let for_person per q350 =
    fresh (q325 c q326 q327 s q328 q343 q332 q333) (q325 === (person c q326 q327 s q328)) (
      c === q332) (q333 === (yellow ())) (per q325) (conde [(q332 === q333) &&& (q343 === (!! true)); (q343 === (!! false)) &&& (q332 =/= q333)])
      (conde
         [(q343 === (!! false)) &&& (q350 === (!! false));
         fresh (q338 q339) (q343 === (!! true)) (s === q338) (q339 === (kools ())) (conde [(q338 === q339) &&& (q350 === (!! true)); (q350 === (!! false)) &&& (q338 =/= q339)])]) in
  any_of_person for_person st q351
let clue09 st q377 =
  fresh (q353 q354 q355 q356 q357 d q358 q359 q360 q361 q365 q366) (q353 === (state q354 q355 (person q356 q357 d q358 q359) q360 q361)) (
    d === q365) (q366 === (milk ())) (st q353) (conde [(q365 === q366) &&& (q377 === (!! true)); (q377 === (!! false)) &&& (q365 =/= q366)])
let clue10 st q403 =
  fresh (q379 q380 n q381 q382 q383 q384 q385 q386 q387 q391 q392) (q379 === (state (person q380 n q381 q382 q383) q384 q385 q386 q387)) (
    n === q391) (q392 === (norwegian ())) (st q379) (conde [(q391 === q392) &&& (q403 === (!! true)); (q403 === (!! false)) &&& (q391 =/= q392)])
let clue11 st q461 =
  let for_neighbors_pair per1 per2 q460 =
    fresh (q405 q406 q407 q408 s1 p1 q410 q411 q412 q413 s2 p2 q448 q428 q417 q418) (
      q405 === (person q406 q407 q408 s1 p1)) (q410 === (person q411 q412 q413 s2 p2)) (
      s1 === q417) (q418 === (chesterfield ())) (per1 q405) (per2 q410) (
      conde [(q417 === q418) &&& (q428 === (!! true)); (q428 === (!! false)) &&& (q417 =/= q418)])
      (conde
         [(q428 === (!! false)) &&& (q448 === (!! false));
         fresh (q423 q424) (q428 === (!! true)) (p2 === q423) (q424 === (fox ())) (conde [(q423 === q424) &&& (q448 === (!! true)); (q448 === (!! false)) &&& (q423 =/= q424)])])
      (conde
         [(q448 === (!! true)) &&& (q460 === (!! true));
         fresh (q444 q433 q434) (q448 === (!! false)) (p1 === q433) (
           q434 === (fox ())) (conde [(q433 === q434) &&& (q444 === (!! true)); (q444 === (!! false)) &&& (q433 =/= q434)])
           (conde
              [(q444 === (!! false)) &&& (q460 === (!! false));
              fresh (q439 q440) (q444 === (!! true)) (s2 === q439) (q440 === (chesterfield ()))
                (conde [(q439 === q440) &&& (q460 === (!! true)); (q460 === (!! false)) &&& (q439 =/= q440)])])]) in
  any_of_neighbors_pair for_neighbors_pair st q461
let clue12 st q519 =
  let for_neighbors_pair per1 per2 q518 =
    fresh (q463 q464 q465 q466 s1 p1 q468 q469 q470 q471 s2 p2 q506 q486 q475 q476) (
      q463 === (person q464 q465 q466 s1 p1)) (q468 === (person q469 q470 q471 s2 p2)) (
      s1 === q475) (q476 === (kools ())) (per1 q463) (per2 q468) (conde [(q475 === q476) &&& (q486 === (!! true)); (q486 === (!! false)) &&& (q475 =/= q476)])
      (conde
         [(q486 === (!! false)) &&& (q506 === (!! false));
         fresh (q481 q482) (q486 === (!! true)) (p2 === q481) (q482 === (hourse ())) (conde [(q481 === q482) &&& (q506 === (!! true)); (q506 === (!! false)) &&& (q481 =/= q482)])])
      (conde
         [(q506 === (!! true)) &&& (q518 === (!! true));
         fresh (q502 q491 q492) (q506 === (!! false)) (p1 === q491) (
           q492 === (hourse ())) (conde [(q491 === q492) &&& (q502 === (!! true)); (q502 === (!! false)) &&& (q491 =/= q492)])
           (conde
              [(q502 === (!! false)) &&& (q518 === (!! false));
              fresh (q497 q498) (q502 === (!! true)) (s2 === q497) (q498 === (kools ()))
                (conde [(q497 === q498) &&& (q518 === (!! true)); (q518 === (!! false)) &&& (q497 =/= q498)])])]) in
  any_of_neighbors_pair for_neighbors_pair st q519
let clue13 st q547 =
  let for_person p q546 =
    fresh (q521 q522 q523 d s q524 q539 q528 q529) (q521 === (person q522 q523 d s q524)) (
      s === q528) (q529 === (lacky_Strike ())) (p q521) (conde [(q528 === q529) &&& (q539 === (!! true)); (q539 === (!! false)) &&& (q528 =/= q529)])
      (conde
         [(q539 === (!! false)) &&& (q546 === (!! false));
         fresh (q534 q535) (q539 === (!! true)) (d === q534) (q535 === (orange_juice ()))
           (conde [(q534 === q535) &&& (q546 === (!! true)); (q546 === (!! false)) &&& (q534 =/= q535)])]) in
  any_of_person for_person st q547
let clue14 st q575 =
  let for_person per q574 =
    fresh (q549 q550 n q551 s q552 q567 q556 q557) (q549 === (person q550 n q551 s q552)) (
      n === q556) (q557 === (japanese ())) (per q549) (conde [(q556 === q557) &&& (q567 === (!! true)); (q567 === (!! false)) &&& (q556 =/= q557)])
      (conde
         [(q567 === (!! false)) &&& (q574 === (!! false));
         fresh (q562 q563) (q567 === (!! true)) (s === q562) (q563 === (parliament ()))
           (conde [(q562 === q563) &&& (q574 === (!! true)); (q574 === (!! false)) &&& (q562 =/= q563)])]) in
  any_of_person for_person st q575
let clue15 st q633 =
  let for_neighbors_pair per1 per2 q632 =
    fresh (q577 c1 n1 q578 q579 q580 q582 c2 n2 q583 q584 q585 q620 q600 q589 q590) (
      q577 === (person c1 n1 q578 q579 q580)) (q582 === (person c2 n2 q583 q584 q585)) (
      n1 === q589) (q590 === (norwegian ())) (per1 q577) (per2 q582) (
      conde [(q589 === q590) &&& (q600 === (!! true)); (q600 === (!! false)) &&& (q589 =/= q590)])
      (conde
         [(q600 === (!! false)) &&& (q620 === (!! false));
         fresh (q595 q596) (q600 === (!! true)) (c2 === q595) (q596 === (blue ())) (conde [(q595 === q596) &&& (q620 === (!! true)); (q620 === (!! false)) &&& (q595 =/= q596)])])
      (conde
         [(q620 === (!! true)) &&& (q632 === (!! true));
         fresh (q616 q605 q606) (q620 === (!! false)) (c1 === q605) (
           q606 === (blue ())) (conde [(q605 === q606) &&& (q616 === (!! true)); (q616 === (!! false)) &&& (q605 =/= q606)])
           (conde
              [(q616 === (!! false)) &&& (q632 === (!! false));
              fresh (q611 q612) (q616 === (!! true)) (n2 === q611) (q612 === (norwegian ()))
                (conde [(q611 === q612) &&& (q632 === (!! true)); (q632 === (!! false)) &&& (q611 =/= q612)])])]) in
  any_of_neighbors_pair for_neighbors_pair st q633
let all_present st q911 =
  let for_person per q887 =
    fresh (q635 c n d s p q880 q680 q639 q640) (q635 === (person c n d s p)) (
      c === q639) (q640 === (yellow ())) (per q635) (conde [(q639 === q640) &&& (q680 === (!! true)); (q680 === (!! false)) &&& (q639 =/= q640)])
      (conde
         [(q680 === (!! true)) &&& (q880 === (!! true));
         fresh (q676 q645 q646) (q680 === (!! false)) (c === q645) (q646 === (blue ()))
           (conde [(q645 === q646) &&& (q676 === (!! true)); (q676 === (!! false)) &&& (q645 =/= q646)])
           (conde
              [(q676 === (!! true)) &&& (q880 === (!! true));
              fresh (q672 q651 q652) (q676 === (!! false)) (c === q651) (
                q652 === (red ())) (conde [(q651 === q652) &&& (q672 === (!! true)); (q672 === (!! false)) &&& (q651 =/= q652)])
                (conde
                   [(q672 === (!! true)) &&& (q880 === (!! true));
                   fresh (q668 q657 q658) (q672 === (!! false)) (c === q657) (
                     q658 === (ivory ())) (conde [(q657 === q658) &&& (q668 === (!! true)); (q668 === (!! false)) &&& (q657 =/= q658)])
                     (conde
                        [(q668 === (!! true)) &&& (q880 === (!! true));
                        fresh (q663 q664) (q668 === (!! false)) (c === q663) (
                          q664 === (green ())) (conde [(q663 === q664) &&& (q880 === (!! true)); (q880 === (!! false)) &&& (q663 =/= q664)])])])])])
      (conde
         [(q880 === (!! false)) &&& (q887 === (!! false));
         fresh (q876 q726 q685 q686) (q880 === (!! true)) (n === q685) (
           q686 === (norwegian ())) (conde [(q685 === q686) &&& (q726 === (!! true)); (q726 === (!! false)) &&& (q685 =/= q686)])
           (conde
              [(q726 === (!! true)) &&& (q876 === (!! true));
              fresh (q722 q691 q692) (q726 === (!! false)) (n === q691) (
                q692 === (ukrainian ())) (conde [(q691 === q692) &&& (q722 === (!! true)); (q722 === (!! false)) &&& (q691 =/= q692)])
                (conde
                   [(q722 === (!! true)) &&& (q876 === (!! true));
                   fresh (q718 q697 q698) (q722 === (!! false)) (n === q697) (
                     q698 === (englishman ())) (conde [(q697 === q698) &&& (q718 === (!! true)); (q718 === (!! false)) &&& (q697 =/= q698)])
                     (conde
                        [(q718 === (!! true)) &&& (q876 === (!! true));
                        fresh (q714 q703 q704) (q718 === (!! false)) (
                          n === q703) (q704 === (spaniard ())) (conde [(q703 === q704) &&& (q714 === (!! true)); (q714 === (!! false)) &&& (q703 =/= q704)])
                          (conde
                             [(q714 === (!! true)) &&& (q876 === (!! true));
                             fresh (q709 q710) (q714 === (!! false)) (
                               n === q709) (q710 === (japanese ())) (
                               conde [(q709 === q710) &&& (q876 === (!! true)); (q876 === (!! false)) &&& (q709 =/= q710)])])])])])
           (conde
              [(q876 === (!! false)) &&& (q887 === (!! false));
              fresh (q872 q772 q731 q732) (q876 === (!! true)) (d === q731) (
                q732 === (water ())) (conde [(q731 === q732) &&& (q772 === (!! true)); (q772 === (!! false)) &&& (q731 =/= q732)])
                (conde
                   [(q772 === (!! true)) &&& (q872 === (!! true));
                   fresh (q768 q737 q738) (q772 === (!! false)) (d === q737) (
                     q738 === (tea ())) (conde [(q737 === q738) &&& (q768 === (!! true)); (q768 === (!! false)) &&& (q737 =/= q738)])
                     (conde
                        [(q768 === (!! true)) &&& (q872 === (!! true));
                        fresh (q764 q743 q744) (q768 === (!! false)) (
                          d === q743) (q744 === (milk ())) (conde [(q743 === q744) &&& (q764 === (!! true)); (q764 === (!! false)) &&& (q743 =/= q744)])
                          (conde
                             [(q764 === (!! true)) &&& (q872 === (!! true));
                             fresh (q760 q749 q750) (q764 === (!! false)) (
                               d === q749) (q750 === (orange_juice ())) (
                               conde [(q749 === q750) &&& (q760 === (!! true)); (q760 === (!! false)) &&& (q749 =/= q750)])
                               (conde
                                  [(q760 === (!! true)) &&& (q872 === (!! true));
                                  fresh (q755 q756) (q760 === (!! false)) (
                                    d === q755) (q756 === (coffee ())) (
                                    conde [(q755 === q756) &&& (q872 === (!! true)); (q872 === (!! false)) &&& (q755 =/= q756)])])])])])
                (conde
                   [(q872 === (!! false)) &&& (q887 === (!! false));
                   fresh (q868 q818 q777 q778) (q872 === (!! true)) (
                     s === q777) (q778 === (kools ())) (conde [(q777 === q778) &&& (q818 === (!! true)); (q818 === (!! false)) &&& (q777 =/= q778)])
                     (conde
                        [(q818 === (!! true)) &&& (q868 === (!! true));
                        fresh (q814 q783 q784) (q818 === (!! false)) (
                          s === q783) (q784 === (chesterfield ())) (conde [(q783 === q784) &&& (q814 === (!! true)); (q814 === (!! false)) &&& (q783 =/= q784)])
                          (conde
                             [(q814 === (!! true)) &&& (q868 === (!! true));
                             fresh (q810 q789 q790) (q814 === (!! false)) (
                               s === q789) (q790 === (old_Gold ())) (
                               conde [(q789 === q790) &&& (q810 === (!! true)); (q810 === (!! false)) &&& (q789 =/= q790)])
                               (conde
                                  [(q810 === (!! true)) &&& (q868 === (!! true));
                                  fresh (q806 q795 q796) (q810 === (!! false)) (
                                    s === q795) (q796 === (lacky_Strike ())) (
                                    conde [(q795 === q796) &&& (q806 === (!! true)); (q806 === (!! false)) &&& (q795 =/= q796)])
                                    (conde
                                       [(q806 === (!! true)) &&& (q868 === (!! true));
                                       fresh (q801 q802) (q806 === (!! false)) (
                                         s === q801) (q802 === (parliament ())) (
                                         conde [(q801 === q802) &&& (q868 === (!! true)); (q868 === (!! false)) &&& (q801 =/= q802)])])])])])
                     (conde
                        [(q868 === (!! false)) &&& (q887 === (!! false));
                        fresh (q864 q823 q824) (q868 === (!! true)) (
                          p === q823) (q824 === (fox ())) (conde [(q823 === q824) &&& (q864 === (!! true)); (q864 === (!! false)) &&& (q823 =/= q824)])
                          (conde
                             [(q864 === (!! true)) &&& (q887 === (!! true));
                             fresh (q860 q829 q830) (q864 === (!! false)) (
                               p === q829) (q830 === (hourse ())) (conde [(q829 === q830) &&& (q860 === (!! true)); (q860 === (!! false)) &&& (q829 =/= q830)])
                               (conde
                                  [(q860 === (!! true)) &&& (q887 === (!! true));
                                  fresh (q856 q835 q836) (q860 === (!! false)) (
                                    p === q835) (q836 === (snails ())) (
                                    conde [(q835 === q836) &&& (q856 === (!! true)); (q856 === (!! false)) &&& (q835 =/= q836)])
                                    (conde
                                       [(q856 === (!! true)) &&& (q887 === (!! true));
                                       fresh (q852 q841 q842) (q856 === (!! false)) (
                                         p === q841) (q842 === (dog ())) (
                                         conde [(q841 === q842) &&& (q852 === (!! true)); (q852 === (!! false)) &&& (q841 =/= q842)])
                                         (conde
                                            [(q852 === (!! true)) &&& (q887 === (!! true));
                                            fresh (q847 q848) (q852 === (!! false)) (
                                              p === q847) (q848 === (zebra ())) (
                                              conde [(q847 === q848) &&& (q887 === (!! true)); (q887 === (!! false)) &&& (q847 =/= q848)])])])])])])])])]) in
  fresh (q889 p1 p2 p3 p4 p5 q904) (q889 === (state p1 p2 p3 p4 p5)) (
    st q889) (for_person (fun q906 -> p1 === q906) q904)
    (conde
       [(q904 === (!! false)) &&& (q911 === (!! false));
       fresh (q900) (q904 === (!! true)) (for_person (fun q907 -> p2 === q907) q900)
         (conde
            [(q900 === (!! false)) &&& (q911 === (!! false));
            fresh (q896) (q900 === (!! true)) (for_person (fun q908 -> p3 === q908) q896)
              (conde
                 [(q896 === (!! false)) &&& (q911 === (!! false));
                 fresh (q892) (q896 === (!! true)) (for_person (fun q909 -> p4 === q909) q892)
                   (conde [(q892 === (!! false)) &&& (q911 === (!! false)); (q892 === (!! true)) &&& (for_person (fun q910 -> p5 === q910) q911)])])])])
let check_state st q972 =
  fresh (q973 q970) (st q973) (all_different (fun q974 -> q974 === q973) q970)
    (conde
       [(q970 === (!! false)) &&& (q972 === (!! false));
       fresh (q966) (q970 === (!! true)) (clue02 (fun q974 -> q974 === q973) q966)
         (conde
            [(q966 === (!! false)) &&& (q972 === (!! false));
            fresh (q962) (q966 === (!! true)) (clue03 (fun q974 -> q974 === q973) q962)
              (conde
                 [(q962 === (!! false)) &&& (q972 === (!! false));
                 fresh (q958) (q962 === (!! true)) (clue04 (fun q974 -> q974 === q973) q958)
                   (conde
                      [(q958 === (!! false)) &&& (q972 === (!! false));
                      fresh (q954) (q958 === (!! true)) (clue05 (fun q974 -> q974 === q973) q954)
                        (conde
                           [(q954 === (!! false)) &&& (q972 === (!! false));
                           fresh (q950) (q954 === (!! true)) (clue06 (fun q974 -> q974 === q973) q950)
                             (conde
                                [(q950 === (!! false)) &&& (q972 === (!! false));
                                fresh (q946) (q950 === (!! true)) (clue07 (fun q974 -> q974 === q973) q946)
                                  (conde
                                     [(q946 === (!! false)) &&& (q972 === (!! false));
                                     fresh (q942) (q946 === (!! true)) (
                                       clue08 (fun q974 -> q974 === q973) q942)
                                       (conde
                                          [(q942 === (!! false)) &&& (q972 === (!! false));
                                          fresh (q938) (q942 === (!! true)) (
                                            clue09 (fun q974 -> q974 === q973) q938)
                                            (conde
                                               [(q938 === (!! false)) &&& (q972 === (!! false));
                                               fresh (q934) (q938 === (!! true)) (
                                                 clue10 (fun q974 -> q974 === q973) q934)
                                                 (conde
                                                    [(q934 === (!! false)) &&& (q972 === (!! false));
                                                    fresh (q930) (q934 === (!! true)) (
                                                      clue11 (fun q974 -> q974 === q973) q930)
                                                      (conde
                                                         [(q930 === (!! false)) &&& (q972 === (!! false));
                                                         fresh (q926) (
                                                           q930 === (!! true)) (
                                                           clue12 (fun q974 -> q974 === q973) q926)
                                                           (conde
                                                              [(q926 === (!! false)) &&& (q972 === (!! false));
                                                              fresh (q922) (
                                                                q926 === (!! true)) (
                                                                clue13 (fun q974 -> q974 === q973) q922)
                                                                (conde
                                                                   [(q922 === (!! false)) &&& (q972 === (!! false));
                                                                   fresh 
                                                                    (q918) (
                                                                    q922 === (!! true)) (
                                                                    clue14 (fun q974 -> q974 === q973) q918)
                                                                    (conde
                                                                    [
                                                                    (q918 === (!! false)) &&& (q972 === (!! false));
                                                                    fresh 
                                                                    (q914) (
                                                                    q918 === (!! true)) (
                                                                    clue15 (fun q974 -> q974 === q973) q914)
                                                                    (conde
                                                                    [
                                                                    (q914 === (!! false)) &&& (q972 === (!! false));
                                                                    (q914 === (!! true)) &&& (all_present (fun q974 -> q974 === q973) q972)])])])])])])])])])])])])])])])