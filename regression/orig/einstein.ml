open GT
open OCanren
open OCanren.Std
type ghause_color =
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
type gnationality =
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
type gdrink =
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
type gsmoke =
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
type gpet =
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
let all_different_o q0 q1 =
  fresh (q2 p1 p2 p3 p4 p5) (q2 === (pair p1 (pair p2 (pair p3 (pair p4 p5))))) (
    q0 q2)
    (let two_different_o a_o b_o q58 =
       fresh (q4 c1 n1 d1 s1 p1 q6 c2 n2 d2 s2 p2 q46 q9 q10) (q4 === (pair c1 (pair n1 (pair d1 (pair s1 p1))))) (
         q6 === (pair c2 (pair n2 (pair d2 (pair s2 p2))))) (c1 === q9) (
         c2 === q10) (a_o q4) (b_o q6) (conde [(q9 === q10) &&& (q46 === (!! false)); (q46 === (!! true)) &&& (q9 =/= q10)])
         (conde
            [(q46 === (!! false)) &&& (q58 === (!! false));
            fresh (q42 q14 q15) (q46 === (!! true)) (n1 === q14) (n2 === q15) (
              conde [(q14 === q15) &&& (q42 === (!! false)); (q42 === (!! true)) &&& (q14 =/= q15)])
              (conde
                 [(q42 === (!! false)) &&& (q58 === (!! false));
                 fresh (q38 q19 q20) (q42 === (!! true)) (d1 === q19) (
                   d2 === q20) (conde [(q19 === q20) &&& (q38 === (!! false)); (q38 === (!! true)) &&& (q19 =/= q20)])
                   (conde
                      [(q38 === (!! false)) &&& (q58 === (!! false));
                      fresh (q34 q24 q25) (q38 === (!! true)) (s1 === q24) (
                        s2 === q25) (conde [(q24 === q25) &&& (q34 === (!! false)); (q34 === (!! true)) &&& (q24 =/= q25)])
                        (conde
                           [(q34 === (!! false)) &&& (q58 === (!! false));
                           fresh (q29 q30) (q34 === (!! true)) (p1 === q29) (p2 === q30) (conde [(q29 === q30) &&& (q58 === (!! false)); (q58 === (!! true)) &&& (q29 =/= q30)])])])])]) in
     fresh (q93) (two_different_o (fun q95 -> p1 === q95) (fun q96 -> p2 === q96) q93)
       (conde
          [(q93 === (!! false)) &&& (q1 === (!! false));
          fresh (q89) (q93 === (!! true)) (two_different_o (fun q95 -> p1 === q95) (fun q97 -> p3 === q97) q89)
            (conde
               [(q89 === (!! false)) &&& (q1 === (!! false));
               fresh (q85) (q89 === (!! true)) (two_different_o (fun q95 -> p1 === q95) (fun q98 -> p4 === q98) q85)
                 (conde
                    [(q85 === (!! false)) &&& (q1 === (!! false));
                    fresh (q81) (q85 === (!! true)) (two_different_o (fun q95 -> p1 === q95) (fun q99 -> p5 === q99) q81)
                      (conde
                         [(q81 === (!! false)) &&& (q1 === (!! false));
                         fresh (q77) (q81 === (!! true)) (two_different_o (fun q96 -> p2 === q96) (fun q97 -> p3 === q97) q77)
                           (conde
                              [(q77 === (!! false)) &&& (q1 === (!! false));
                              fresh (q73) (q77 === (!! true)) (two_different_o (fun q96 -> p2 === q96) (fun q98 -> p4 === q98) q73)
                                (conde
                                   [(q73 === (!! false)) &&& (q1 === (!! false));
                                   fresh (q69) (q73 === (!! true)) (two_different_o (fun q96 -> p2 === q96) (fun q99 -> p5 === q99) q69)
                                     (conde
                                        [(q69 === (!! false)) &&& (q1 === (!! false));
                                        fresh (q65) (q69 === (!! true)) (
                                          two_different_o (fun q97 -> p3 === q97) (fun q98 -> p4 === q98) q65)
                                          (conde
                                             [(q65 === (!! false)) &&& (q1 === (!! false));
                                             fresh (q61) (q65 === (!! true)) (
                                               two_different_o (fun q97 -> p3 === q97) (fun q99 -> p5 === q99) q61)
                                               (conde
                                                  [(q61 === (!! false)) &&& (q1 === (!! false));
                                                  (q61 === (!! true)) &&& (two_different_o (fun q98 -> p4 === q98) (fun q99 -> p5 === q99) q1)])])])])])])])])]))
let any_of_person_o f_o q100 q125 =
  fresh (q103 p1 p2 p3 p4 p5 q118) (q103 === (pair p1 (pair p2 (pair p3 (pair p4 p5))))) (
    q100 q103) (f_o (fun q120 -> p1 === q120) q118)
    (conde
       [(q118 === (!! true)) &&& (q125 === (!! true));
       fresh (q114) (q118 === (!! false)) (f_o (fun q121 -> p2 === q121) q114)
         (conde
            [(q114 === (!! true)) &&& (q125 === (!! true));
            fresh (q110) (q114 === (!! false)) (f_o (fun q122 -> p3 === q122) q110)
              (conde
                 [(q110 === (!! true)) &&& (q125 === (!! true));
                 fresh (q106) (q110 === (!! false)) (f_o (fun q123 -> p4 === q123) q106)
                   (conde [(q106 === (!! true)) &&& (q125 === (!! true)); (q106 === (!! false)) &&& (f_o (fun q124 -> p5 === q124) q125)])])])])
let any_of_neighbors_pair_o f_o q126 q147 =
  fresh (q129 p1 p2 p3 p4 p5 q140) (q129 === (pair p1 (pair p2 (pair p3 (pair p4 p5))))) (
    q126 q129) (f_o (fun q142 -> p1 === q142) (fun q143 -> p2 === q143) q140)
    (conde
       [(q140 === (!! true)) &&& (q147 === (!! true));
       fresh (q136) (q140 === (!! false)) (f_o (fun q143 -> p2 === q143) (fun q144 -> p3 === q144) q136)
         (conde
            [(q136 === (!! true)) &&& (q147 === (!! true));
            fresh (q132) (q136 === (!! false)) (f_o (fun q144 -> p3 === q144) (fun q145 -> p4 === q145) q132)
              (conde [(q132 === (!! true)) &&& (q147 === (!! true)); (q132 === (!! false)) &&& (f_o (fun q145 -> p4 === q145) (fun q146 -> p5 === q146) q147)])])])
let clue02_o st_o q175 =
  let for_person_o q148 q149 =
    fresh (q150 c n q151 q152 q153 q168 q157 q158) (q150 === (pair c (pair n (pair q151 (pair q152 q153))))) (
      n === q157) (q158 === (englishman ())) (q148 q150) (conde [(q157 === q158) &&& (q168 === (!! true)); (q168 === (!! false)) &&& (q157 =/= q158)])
      (conde
         [(q168 === (!! false)) &&& (q149 === (!! false));
         fresh (q163 q164) (q168 === (!! true)) (c === q163) (q164 === (red ())) (conde [(q163 === q164) &&& (q149 === (!! true)); (q149 === (!! false)) &&& (q163 =/= q164)])]) in
  any_of_person_o for_person_o st_o q175
let clue03_o st_o q203 =
  let for_person_o q176 q177 =
    fresh (q178 q179 n q180 q181 p q196 q185 q186) (q178 === (pair q179 (pair n (pair q180 (pair q181 p))))) (
      n === q185) (q186 === (spaniard ())) (q176 q178) (conde [(q185 === q186) &&& (q196 === (!! true)); (q196 === (!! false)) &&& (q185 =/= q186)])
      (conde
         [(q196 === (!! false)) &&& (q177 === (!! false));
         fresh (q191 q192) (q196 === (!! true)) (p === q191) (q192 === (dog ())) (conde [(q191 === q192) &&& (q177 === (!! true)); (q177 === (!! false)) &&& (q191 =/= q192)])]) in
  any_of_person_o for_person_o st_o q203
let clue04_o st_o q231 =
  let for_person_o q204 q205 =
    fresh (q206 c q207 d q208 q209 q224 q213 q214) (q206 === (pair c (pair q207 (pair d (pair q208 q209))))) (
      c === q213) (q214 === (green ())) (q204 q206) (conde [(q213 === q214) &&& (q224 === (!! true)); (q224 === (!! false)) &&& (q213 =/= q214)])
      (conde
         [(q224 === (!! false)) &&& (q205 === (!! false));
         fresh (q219 q220) (q224 === (!! true)) (d === q219) (q220 === (coffee ())) (conde [(q219 === q220) &&& (q205 === (!! true)); (q205 === (!! false)) &&& (q219 =/= q220)])]) in
  any_of_person_o for_person_o st_o q231
let clue05_o st_o q259 =
  let for_person_o q232 q233 =
    fresh (q234 q235 n d q236 q237 q252 q241 q242) (q234 === (pair q235 (pair n (pair d (pair q236 q237))))) (
      n === q241) (q242 === (ukrainian ())) (q232 q234) (conde [(q241 === q242) &&& (q252 === (!! true)); (q252 === (!! false)) &&& (q241 =/= q242)])
      (conde
         [(q252 === (!! false)) &&& (q233 === (!! false));
         fresh (q247 q248) (q252 === (!! true)) (d === q247) (q248 === (tea ())) (conde [(q247 === q248) &&& (q233 === (!! true)); (q233 === (!! false)) &&& (q247 =/= q248)])]) in
  any_of_person_o for_person_o st_o q259
let clue06_o st_o q301 =
  let for_neighbors_pair_o q260 q261 q262 =
    fresh (q263 c1 q264 q265 q266 q267 q270 c2 q271 q272 q273 q274 q289 q278 q279) (
      q263 === (pair c1 (pair q264 (pair q265 (pair q266 q267))))) (q270 === (pair c2 (pair q271 (pair q272 (pair q273 q274))))) (
      c1 === q278) (q279 === (ivory ())) (q260 q263) (q261 q270) (conde [(q278 === q279) &&& (q289 === (!! true)); (q289 === (!! false)) &&& (q278 =/= q279)])
      (conde
         [(q289 === (!! false)) &&& (q262 === (!! false));
         fresh (q284 q285) (q289 === (!! true)) (c2 === q284) (q285 === (green ())) (conde [(q284 === q285) &&& (q262 === (!! true)); (q262 === (!! false)) &&& (q284 =/= q285)])]) in
  any_of_neighbors_pair_o for_neighbors_pair_o st_o q301
let clue07_o st_o q329 =
  let for_person_o q302 q303 =
    fresh (q304 q305 q306 q307 s p q322 q311 q312) (q304 === (pair q305 (pair q306 (pair q307 (pair s p))))) (
      s === q311) (q312 === (old_Gold ())) (q302 q304) (conde [(q311 === q312) &&& (q322 === (!! true)); (q322 === (!! false)) &&& (q311 =/= q312)])
      (conde
         [(q322 === (!! false)) &&& (q303 === (!! false));
         fresh (q317 q318) (q322 === (!! true)) (p === q317) (q318 === (snails ())) (conde [(q317 === q318) &&& (q303 === (!! true)); (q303 === (!! false)) &&& (q317 =/= q318)])]) in
  any_of_person_o for_person_o st_o q329
let clue08_o st_o q357 =
  let for_person_o q330 q331 =
    fresh (q332 c q333 q334 s q335 q350 q339 q340) (q332 === (pair c (pair q333 (pair q334 (pair s q335))))) (
      c === q339) (q340 === (yellow ())) (q330 q332) (conde [(q339 === q340) &&& (q350 === (!! true)); (q350 === (!! false)) &&& (q339 =/= q340)])
      (conde
         [(q350 === (!! false)) &&& (q331 === (!! false));
         fresh (q345 q346) (q350 === (!! true)) (s === q345) (q346 === (kools ())) (conde [(q345 === q346) &&& (q331 === (!! true)); (q331 === (!! false)) &&& (q345 =/= q346)])]) in
  any_of_person_o for_person_o st_o q357
let clue09_o q358 q359 =
  fresh (q360 q361 q362 q363 q364 d q365 q366 q367 q368 q372 q373) (q360 === (pair q361 (pair q362 (pair (pair q363 (pair q364 (pair d (pair q365 q366)))) (pair q367 q368)))))
    (d === q372) (q373 === (milk ())) (q358 q360) (conde [(q372 === q373) &&& (q359 === (!! true)); (q359 === (!! false)) &&& (q372 =/= q373)])
let clue10_o q384 q385 =
  fresh (q386 q387 n q388 q389 q390 q391 q392 q393 q394 q398 q399) (q386 === (pair (pair q387 (pair n (pair q388 (pair q389 q390)))) (pair q391 (pair q392 (pair q393 q394)))))
    (n === q398) (q399 === (norwegian ())) (q384 q386) (conde [(q398 === q399) &&& (q385 === (!! true)); (q385 === (!! false)) &&& (q398 =/= q399)])
let clue11_o st_o q469 =
  let for_neighbors_pair_o q410 q411 q412 =
    fresh (q413 q414 q415 q416 s1 p1 q419 q420 q421 q422 s2 p2 q457 q437 q426 q427) (
      q413 === (pair q414 (pair q415 (pair q416 (pair s1 p1))))) (q419 === (pair q420 (pair q421 (pair q422 (pair s2 p2))))) (
      s1 === q426) (q427 === (chesterfield ())) (q410 q413) (q411 q419) (
      conde [(q426 === q427) &&& (q437 === (!! true)); (q437 === (!! false)) &&& (q426 =/= q427)])
      (conde
         [(q437 === (!! false)) &&& (q457 === (!! false));
         fresh (q432 q433) (q437 === (!! true)) (p2 === q432) (q433 === (fox ())) (conde [(q432 === q433) &&& (q457 === (!! true)); (q457 === (!! false)) &&& (q432 =/= q433)])])
      (conde
         [(q457 === (!! true)) &&& (q412 === (!! true));
         fresh (q453 q442 q443) (q457 === (!! false)) (p1 === q442) (
           q443 === (fox ())) (conde [(q442 === q443) &&& (q453 === (!! true)); (q453 === (!! false)) &&& (q442 =/= q443)])
           (conde
              [(q453 === (!! false)) &&& (q412 === (!! false));
              fresh (q448 q449) (q453 === (!! true)) (s2 === q448) (q449 === (chesterfield ()))
                (conde [(q448 === q449) &&& (q412 === (!! true)); (q412 === (!! false)) &&& (q448 =/= q449)])])]) in
  any_of_neighbors_pair_o for_neighbors_pair_o st_o q469
let clue12_o st_o q529 =
  let for_neighbors_pair_o q470 q471 q472 =
    fresh (q473 q474 q475 q476 s1 p1 q479 q480 q481 q482 s2 p2 q517 q497 q486 q487) (
      q473 === (pair q474 (pair q475 (pair q476 (pair s1 p1))))) (q479 === (pair q480 (pair q481 (pair q482 (pair s2 p2))))) (
      s1 === q486) (q487 === (kools ())) (q470 q473) (q471 q479) (conde [(q486 === q487) &&& (q497 === (!! true)); (q497 === (!! false)) &&& (q486 =/= q487)])
      (conde
         [(q497 === (!! false)) &&& (q517 === (!! false));
         fresh (q492 q493) (q497 === (!! true)) (p2 === q492) (q493 === (hourse ())) (conde [(q492 === q493) &&& (q517 === (!! true)); (q517 === (!! false)) &&& (q492 =/= q493)])])
      (conde
         [(q517 === (!! true)) &&& (q472 === (!! true));
         fresh (q513 q502 q503) (q517 === (!! false)) (p1 === q502) (
           q503 === (hourse ())) (conde [(q502 === q503) &&& (q513 === (!! true)); (q513 === (!! false)) &&& (q502 =/= q503)])
           (conde
              [(q513 === (!! false)) &&& (q472 === (!! false));
              fresh (q508 q509) (q513 === (!! true)) (s2 === q508) (q509 === (kools ()))
                (conde [(q508 === q509) &&& (q472 === (!! true)); (q472 === (!! false)) &&& (q508 =/= q509)])])]) in
  any_of_neighbors_pair_o for_neighbors_pair_o st_o q529
let clue13_o st_o q557 =
  let for_person_o q530 q531 =
    fresh (q532 q533 q534 d s q535 q550 q539 q540) (q532 === (pair q533 (pair q534 (pair d (pair s q535))))) (
      s === q539) (q540 === (lacky_Strike ())) (q530 q532) (conde [(q539 === q540) &&& (q550 === (!! true)); (q550 === (!! false)) &&& (q539 =/= q540)])
      (conde
         [(q550 === (!! false)) &&& (q531 === (!! false));
         fresh (q545 q546) (q550 === (!! true)) (d === q545) (q546 === (orange_juice ()))
           (conde [(q545 === q546) &&& (q531 === (!! true)); (q531 === (!! false)) &&& (q545 =/= q546)])]) in
  any_of_person_o for_person_o st_o q557
let clue14_o st_o q585 =
  let for_person_o q558 q559 =
    fresh (q560 q561 n q562 s q563 q578 q567 q568) (q560 === (pair q561 (pair n (pair q562 (pair s q563))))) (
      n === q567) (q568 === (japanese ())) (q558 q560) (conde [(q567 === q568) &&& (q578 === (!! true)); (q578 === (!! false)) &&& (q567 =/= q568)])
      (conde
         [(q578 === (!! false)) &&& (q559 === (!! false));
         fresh (q573 q574) (q578 === (!! true)) (s === q573) (q574 === (parliament ()))
           (conde [(q573 === q574) &&& (q559 === (!! true)); (q559 === (!! false)) &&& (q573 =/= q574)])]) in
  any_of_person_o for_person_o st_o q585
let clue15_o st_o q645 =
  let for_neighbors_pair_o q586 q587 q588 =
    fresh (q589 c1 n1 q590 q591 q592 q595 c2 n2 q596 q597 q598 q633 q613 q602 q603) (
      q589 === (pair c1 (pair n1 (pair q590 (pair q591 q592))))) (q595 === (pair c2 (pair n2 (pair q596 (pair q597 q598))))) (
      n1 === q602) (q603 === (norwegian ())) (q586 q589) (q587 q595) (
      conde [(q602 === q603) &&& (q613 === (!! true)); (q613 === (!! false)) &&& (q602 =/= q603)])
      (conde
         [(q613 === (!! false)) &&& (q633 === (!! false));
         fresh (q608 q609) (q613 === (!! true)) (c2 === q608) (q609 === (blue ())) (conde [(q608 === q609) &&& (q633 === (!! true)); (q633 === (!! false)) &&& (q608 =/= q609)])])
      (conde
         [(q633 === (!! true)) &&& (q588 === (!! true));
         fresh (q629 q618 q619) (q633 === (!! false)) (c1 === q618) (
           q619 === (blue ())) (conde [(q618 === q619) &&& (q629 === (!! true)); (q629 === (!! false)) &&& (q618 =/= q619)])
           (conde
              [(q629 === (!! false)) &&& (q588 === (!! false));
              fresh (q624 q625) (q629 === (!! true)) (n2 === q624) (q625 === (norwegian ()))
                (conde [(q624 === q625) &&& (q588 === (!! true)); (q588 === (!! false)) &&& (q624 =/= q625)])])]) in
  any_of_neighbors_pair_o for_neighbors_pair_o st_o q645
let all_present_o q646 q647 =
  fresh (q648 p1 p2 p3 p4 p5) (q648 === (pair p1 (pair p2 (pair p3 (pair p4 p5))))) (
    q646 q648)
    (let for_person_o q649 q650 =
       fresh (q651 c n d s p q896 q696 q655 q656) (q651 === (pair c (pair n (pair d (pair s p))))) (
         c === q655) (q656 === (yellow ())) (q649 q651) (conde [(q655 === q656) &&& (q696 === (!! true)); (q696 === (!! false)) &&& (q655 =/= q656)])
         (conde
            [(q696 === (!! true)) &&& (q896 === (!! true));
            fresh (q692 q661 q662) (q696 === (!! false)) (c === q661) (
              q662 === (blue ())) (conde [(q661 === q662) &&& (q692 === (!! true)); (q692 === (!! false)) &&& (q661 =/= q662)])
              (conde
                 [(q692 === (!! true)) &&& (q896 === (!! true));
                 fresh (q688 q667 q668) (q692 === (!! false)) (c === q667) (
                   q668 === (red ())) (conde [(q667 === q668) &&& (q688 === (!! true)); (q688 === (!! false)) &&& (q667 =/= q668)])
                   (conde
                      [(q688 === (!! true)) &&& (q896 === (!! true));
                      fresh (q684 q673 q674) (q688 === (!! false)) (c === q673) (
                        q674 === (ivory ())) (conde [(q673 === q674) &&& (q684 === (!! true)); (q684 === (!! false)) &&& (q673 =/= q674)])
                        (conde
                           [(q684 === (!! true)) &&& (q896 === (!! true));
                           fresh (q679 q680) (q684 === (!! false)) (c === q679) (
                             q680 === (green ())) (conde [(q679 === q680) &&& (q896 === (!! true)); (q896 === (!! false)) &&& (q679 =/= q680)])])])])])
         (conde
            [(q896 === (!! false)) &&& (q650 === (!! false));
            fresh (q892 q742 q701 q702) (q896 === (!! true)) (n === q701) (
              q702 === (norwegian ())) (conde [(q701 === q702) &&& (q742 === (!! true)); (q742 === (!! false)) &&& (q701 =/= q702)])
              (conde
                 [(q742 === (!! true)) &&& (q892 === (!! true));
                 fresh (q738 q707 q708) (q742 === (!! false)) (n === q707) (
                   q708 === (ukrainian ())) (conde [(q707 === q708) &&& (q738 === (!! true)); (q738 === (!! false)) &&& (q707 =/= q708)])
                   (conde
                      [(q738 === (!! true)) &&& (q892 === (!! true));
                      fresh (q734 q713 q714) (q738 === (!! false)) (n === q713) (
                        q714 === (englishman ())) (conde [(q713 === q714) &&& (q734 === (!! true)); (q734 === (!! false)) &&& (q713 =/= q714)])
                        (conde
                           [(q734 === (!! true)) &&& (q892 === (!! true));
                           fresh (q730 q719 q720) (q734 === (!! false)) (
                             n === q719) (q720 === (spaniard ())) (conde [(q719 === q720) &&& (q730 === (!! true)); (q730 === (!! false)) &&& (q719 =/= q720)])
                             (conde
                                [(q730 === (!! true)) &&& (q892 === (!! true));
                                fresh (q725 q726) (q730 === (!! false)) (
                                  n === q725) (q726 === (japanese ())) (
                                  conde [(q725 === q726) &&& (q892 === (!! true)); (q892 === (!! false)) &&& (q725 =/= q726)])])])])])
              (conde
                 [(q892 === (!! false)) &&& (q650 === (!! false));
                 fresh (q888 q788 q747 q748) (q892 === (!! true)) (d === q747) (
                   q748 === (water ())) (conde [(q747 === q748) &&& (q788 === (!! true)); (q788 === (!! false)) &&& (q747 =/= q748)])
                   (conde
                      [(q788 === (!! true)) &&& (q888 === (!! true));
                      fresh (q784 q753 q754) (q788 === (!! false)) (d === q753) (
                        q754 === (tea ())) (conde [(q753 === q754) &&& (q784 === (!! true)); (q784 === (!! false)) &&& (q753 =/= q754)])
                        (conde
                           [(q784 === (!! true)) &&& (q888 === (!! true));
                           fresh (q780 q759 q760) (q784 === (!! false)) (
                             d === q759) (q760 === (milk ())) (conde [(q759 === q760) &&& (q780 === (!! true)); (q780 === (!! false)) &&& (q759 =/= q760)])
                             (conde
                                [(q780 === (!! true)) &&& (q888 === (!! true));
                                fresh (q776 q765 q766) (q780 === (!! false)) (
                                  d === q765) (q766 === (orange_juice ())) (
                                  conde [(q765 === q766) &&& (q776 === (!! true)); (q776 === (!! false)) &&& (q765 =/= q766)])
                                  (conde
                                     [(q776 === (!! true)) &&& (q888 === (!! true));
                                     fresh (q771 q772) (q776 === (!! false)) (
                                       d === q771) (q772 === (coffee ())) (
                                       conde [(q771 === q772) &&& (q888 === (!! true)); (q888 === (!! false)) &&& (q771 =/= q772)])])])])])
                   (conde
                      [(q888 === (!! false)) &&& (q650 === (!! false));
                      fresh (q884 q834 q793 q794) (q888 === (!! true)) (
                        s === q793) (q794 === (kools ())) (conde [(q793 === q794) &&& (q834 === (!! true)); (q834 === (!! false)) &&& (q793 =/= q794)])
                        (conde
                           [(q834 === (!! true)) &&& (q884 === (!! true));
                           fresh (q830 q799 q800) (q834 === (!! false)) (
                             s === q799) (q800 === (chesterfield ())) (
                             conde [(q799 === q800) &&& (q830 === (!! true)); (q830 === (!! false)) &&& (q799 =/= q800)])
                             (conde
                                [(q830 === (!! true)) &&& (q884 === (!! true));
                                fresh (q826 q805 q806) (q830 === (!! false)) (
                                  s === q805) (q806 === (old_Gold ())) (
                                  conde [(q805 === q806) &&& (q826 === (!! true)); (q826 === (!! false)) &&& (q805 =/= q806)])
                                  (conde
                                     [(q826 === (!! true)) &&& (q884 === (!! true));
                                     fresh (q822 q811 q812) (q826 === (!! false)) (
                                       s === q811) (q812 === (lacky_Strike ())) (
                                       conde [(q811 === q812) &&& (q822 === (!! true)); (q822 === (!! false)) &&& (q811 =/= q812)])
                                       (conde
                                          [(q822 === (!! true)) &&& (q884 === (!! true));
                                          fresh (q817 q818) (q822 === (!! false)) (
                                            s === q817) (q818 === (parliament ())) (
                                            conde [(q817 === q818) &&& (q884 === (!! true)); (q884 === (!! false)) &&& (q817 =/= q818)])])])])])
                        (conde
                           [(q884 === (!! false)) &&& (q650 === (!! false));
                           fresh (q880 q839 q840) (q884 === (!! true)) (
                             p === q839) (q840 === (fox ())) (conde [(q839 === q840) &&& (q880 === (!! true)); (q880 === (!! false)) &&& (q839 =/= q840)])
                             (conde
                                [(q880 === (!! true)) &&& (q650 === (!! true));
                                fresh (q876 q845 q846) (q880 === (!! false)) (
                                  p === q845) (q846 === (hourse ())) (
                                  conde [(q845 === q846) &&& (q876 === (!! true)); (q876 === (!! false)) &&& (q845 =/= q846)])
                                  (conde
                                     [(q876 === (!! true)) &&& (q650 === (!! true));
                                     fresh (q872 q851 q852) (q876 === (!! false)) (
                                       p === q851) (q852 === (snails ())) (
                                       conde [(q851 === q852) &&& (q872 === (!! true)); (q872 === (!! false)) &&& (q851 =/= q852)])
                                       (conde
                                          [(q872 === (!! true)) &&& (q650 === (!! true));
                                          fresh (q868 q857 q858) (q872 === (!! false)) (
                                            p === q857) (q858 === (dog ())) (
                                            conde [(q857 === q858) &&& (q868 === (!! true)); (q868 === (!! false)) &&& (q857 =/= q858)])
                                            (conde
                                               [(q868 === (!! true)) &&& (q650 === (!! true));
                                               fresh (q863 q864) (q868 === (!! false)) (
                                                 p === q863) (q864 === (zebra ())) (
                                                 conde [(q863 === q864) &&& (q650 === (!! true)); (q650 === (!! false)) &&& (q863 =/= q864)])])])])])])])])]) in
     fresh (q917) (for_person_o (fun q919 -> p1 === q919) q917)
       (conde
          [(q917 === (!! false)) &&& (q647 === (!! false));
          fresh (q913) (q917 === (!! true)) (for_person_o (fun q920 -> p2 === q920) q913)
            (conde
               [(q913 === (!! false)) &&& (q647 === (!! false));
               fresh (q909) (q913 === (!! true)) (for_person_o (fun q921 -> p3 === q921) q909)
                 (conde
                    [(q909 === (!! false)) &&& (q647 === (!! false));
                    fresh (q905) (q909 === (!! true)) (for_person_o (fun q922 -> p4 === q922) q905)
                      (conde [(q905 === (!! false)) &&& (q647 === (!! false)); (q905 === (!! true)) &&& (for_person_o (fun q923 -> p5 === q923) q647)])])])]))
let check_state_o st_o q984 =
  fresh (q985 q982) (st_o q985) (all_different_o (fun q986 -> q986 === q985) q982)
    (conde
       [(q982 === (!! false)) &&& (q984 === (!! false));
       fresh (q978) (q982 === (!! true)) (clue02_o (fun q986 -> q986 === q985) q978)
         (conde
            [(q978 === (!! false)) &&& (q984 === (!! false));
            fresh (q974) (q978 === (!! true)) (clue03_o (fun q986 -> q986 === q985) q974)
              (conde
                 [(q974 === (!! false)) &&& (q984 === (!! false));
                 fresh (q970) (q974 === (!! true)) (clue04_o (fun q986 -> q986 === q985) q970)
                   (conde
                      [(q970 === (!! false)) &&& (q984 === (!! false));
                      fresh (q966) (q970 === (!! true)) (clue05_o (fun q986 -> q986 === q985) q966)
                        (conde
                           [(q966 === (!! false)) &&& (q984 === (!! false));
                           fresh (q962) (q966 === (!! true)) (clue06_o (fun q986 -> q986 === q985) q962)
                             (conde
                                [(q962 === (!! false)) &&& (q984 === (!! false));
                                fresh (q958) (q962 === (!! true)) (clue07_o (fun q986 -> q986 === q985) q958)
                                  (conde
                                     [(q958 === (!! false)) &&& (q984 === (!! false));
                                     fresh (q954) (q958 === (!! true)) (
                                       clue08_o (fun q986 -> q986 === q985) q954)
                                       (conde
                                          [(q954 === (!! false)) &&& (q984 === (!! false));
                                          fresh (q950) (q954 === (!! true)) (
                                            clue09_o (fun q986 -> q986 === q985) q950)
                                            (conde
                                               [(q950 === (!! false)) &&& (q984 === (!! false));
                                               fresh (q946) (q950 === (!! true)) (
                                                 clue10_o (fun q986 -> q986 === q985) q946)
                                                 (conde
                                                    [(q946 === (!! false)) &&& (q984 === (!! false));
                                                    fresh (q942) (q946 === (!! true)) (
                                                      clue11_o (fun q986 -> q986 === q985) q942)
                                                      (conde
                                                         [(q942 === (!! false)) &&& (q984 === (!! false));
                                                         fresh (q938) (
                                                           q942 === (!! true)) (
                                                           clue12_o (fun q986 -> q986 === q985) q938)
                                                           (conde
                                                              [(q938 === (!! false)) &&& (q984 === (!! false));
                                                              fresh (q934) (
                                                                q938 === (!! true)) (
                                                                clue13_o (fun q986 -> q986 === q985) q934)
                                                                (conde
                                                                   [(q934 === (!! false)) &&& (q984 === (!! false));
                                                                   fresh 
                                                                    (q930) (
                                                                    q934 === (!! true)) (
                                                                    clue14_o (fun q986 -> q986 === q985) q930)
                                                                    (conde
                                                                    [
                                                                    (q930 === (!! false)) &&& (q984 === (!! false));
                                                                    fresh 
                                                                    (q926) (
                                                                    q930 === (!! true)) (
                                                                    clue15_o (fun q986 -> q986 === q985) q926)
                                                                    (conde
                                                                    [
                                                                    (q926 === (!! false)) &&& (q984 === (!! false));
                                                                    (q926 === (!! true)) &&& (all_present_o (fun q986 -> q986 === q985) q984)])])])])])])])])])])])])])])])