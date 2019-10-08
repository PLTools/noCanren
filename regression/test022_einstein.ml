open GT
open OCanren
open OCanren.Std
open Tester

let conj = (<&>)
let (&&&) = (<&>)

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
let all_different st q776 =
  let two_different a b q680 =
    fresh (c1 n1 d1 s1 p1 c2 n2 d2 s2 p2 q682 q683 q691 q692 q700 q701 q709 q710) (
      a === (person c1 n1 d1 s1 p1)) (b === (person c2 n2 d2 s2 p2)) (
      conde [(c1 === c2) &&& (q682 === (!! false)); (q682 === (!! true)) &&& (c1 =/= c2)]) (
      conde [(n1 === n2) &&& (q691 === (!! false)); (q691 === (!! true)) &&& (n1 =/= n2)]) (
      conde [(d1 === d2) &&& (q700 === (!! false)); (q700 === (!! true)) &&& (d1 =/= d2)]) (
      conde [(s1 === s2) &&& (q709 === (!! false)); (q709 === (!! true)) &&& (s1 =/= s2)]) (
      conde [(p1 === p2) &&& (q710 === (!! false)); (q710 === (!! true)) &&& (p1 =/= p2)])
      (conde [(q709 === (!! false)) &&& (q701 === (!! false)); (q709 === (!! true)) &&& (q701 === q710)])
      (conde [(q700 === (!! false)) &&& (q692 === (!! false)); (q700 === (!! true)) &&& (q692 === q701)])
      (conde [(q691 === (!! false)) &&& (q683 === (!! false)); (q691 === (!! true)) &&& (q683 === q692)])
      (conde [(q682 === (!! false)) &&& (q680 === (!! false)); (q682 === (!! true)) &&& (q680 === q683)]) in
  fresh (p1 p2 p3 p4 p5 q722 q723 q728 q729 q734 q735 q740 q741 q746 q747 q752 q753 q758 q759 q764 q765 q770 q771) 
    (two_different p1 p2 q722) (two_different p1 p3 q728) (
    two_different p1 p4 q734) (two_different p1 p5 q740) (two_different p2 p3 q746) (
    two_different p2 p4 q752) (two_different p2 p5 q758) (two_different p3 p4 q764) (
    two_different p3 p5 q770) (two_different p4 p5 q771)
    (st === (state p1 p2 p3 p4 p5))  
    (conde [(q770 === (!! false)) &&& (q765 === (!! false)); (q770 === (!! true)) &&& (q765 === q771)])
    (conde [(q764 === (!! false)) &&& (q759 === (!! false)); (q764 === (!! true)) &&& (q759 === q765)])
    (conde [(q758 === (!! false)) &&& (q753 === (!! false)); (q758 === (!! true)) &&& (q753 === q759)])
    (conde [(q752 === (!! false)) &&& (q747 === (!! false)); (q752 === (!! true)) &&& (q747 === q753)])
    (conde [(q746 === (!! false)) &&& (q741 === (!! false)); (q746 === (!! true)) &&& (q741 === q747)])
    (conde [(q740 === (!! false)) &&& (q735 === (!! false)); (q740 === (!! true)) &&& (q735 === q741)])
    (conde [(q734 === (!! false)) &&& (q729 === (!! false)); (q734 === (!! true)) &&& (q729 === q735)])
    (conde [(q728 === (!! false)) &&& (q723 === (!! false)); (q728 === (!! true)) &&& (q723 === q729)])
    (conde [(q722 === (!! false)) &&& (q776 === (!! false)); (q722 === (!! true)) &&& (q776 === q723)])

let any_of_person f st q655 =
  fresh (p1 p2 p3 p4 p5 q656 q657 q662 q663 q668 q669 q674 q675) (st === (state p1 p2 p3 p4 p5)) (
    f p1 q656) (f p2 q662) (f p3 q668) (f p4 q674) (f p5 q675) (conde [(q674 === (!! true)) &&& (q669 === (!! true)); (q674 === (!! false)) &&& (q669 === q675)])
    (conde [(q668 === (!! true)) &&& (q663 === (!! true)); (q668 === (!! false)) &&& (q663 === q669)])
    (conde [(q662 === (!! true)) &&& (q657 === (!! true)); (q662 === (!! false)) &&& (q657 === q663)])
    (conde [(q656 === (!! true)) &&& (q655 === (!! true)); (q656 === (!! false)) &&& (q655 === q657)])
let any_of_neighbors_pair f st q636 =
  fresh (p1 p2 p3 p4 p5 q637 q638 q643 q644 q649 q650) (st === (state p1 p2 p3 p4 p5)) (
    f p1 p2 q637) (f p2 p3 q643) (f p3 p4 q649) (f p4 p5 q650) (conde [(q649 === (!! true)) &&& (q644 === (!! true)); (q649 === (!! false)) &&& (q644 === q650)])
    (conde [(q643 === (!! true)) &&& (q638 === (!! true)); (q643 === (!! false)) &&& (q638 === q644)])
    (conde [(q637 === (!! true)) &&& (q636 === (!! true)); (q637 === (!! false)) &&& (q636 === q638)])
let clue02 st q635 =
  let for_person per q619 =
    fresh (c n q620 q621 q622 q623 q624) (per === (person c n q620 q621 q622))
      (conde [(n === (englishman ())) &&& (q623 === (!! true)); (q623 === (!! false)) &&& (n =/= (englishman ()))])
      (conde [(c === (red ())) &&& (q624 === (!! true)); (q624 === (!! false)) &&& (c =/= (red ()))])
      (conde [(q623 === (!! false)) &&& (q619 === (!! false)); (q623 === (!! true)) &&& (q619 === q624)]) in
  any_of_person for_person st q635
let clue03 st q618 =
  let for_person per q602 =
    fresh (q603 n q604 q605 p q606 q607) (per === (person q603 n q604 q605 p))
      (conde [(n === (spaniard ())) &&& (q606 === (!! true)); (q606 === (!! false)) &&& (n =/= (spaniard ()))])
      (conde [(p === (dog ())) &&& (q607 === (!! true)); (q607 === (!! false)) &&& (p =/= (dog ()))])
      (conde [(q606 === (!! false)) &&& (q602 === (!! false)); (q606 === (!! true)) &&& (q602 === q607)]) in
  any_of_person for_person st q618
let clue04 st q601 =
  let for_person per q585 =
    fresh (c q586 d q587 q588 q589 q590) (per === (person c q586 d q587 q588)) (
      conde [(c === (green ())) &&& (q589 === (!! true)); (q589 === (!! false)) &&& (c =/= (green ()))])
      (conde [(d === (coffee ())) &&& (q590 === (!! true)); (q590 === (!! false)) &&& (d =/= (coffee ()))])
      (conde [(q589 === (!! false)) &&& (q585 === (!! false)); (q589 === (!! true)) &&& (q585 === q590)]) in
  any_of_person for_person st q601
let clue05 st q584 =
  let for_person per q568 =
    fresh (q569 n d q570 q571 q572 q573) (per === (person q569 n d q570 q571))
      (conde [(n === (ukrainian ())) &&& (q572 === (!! true)); (q572 === (!! false)) &&& (n =/= (ukrainian ()))])
      (conde [(d === (tea ())) &&& (q573 === (!! true)); (q573 === (!! false)) &&& (d =/= (tea ()))])
      (conde [(q572 === (!! false)) &&& (q568 === (!! false)); (q572 === (!! true)) &&& (q568 === q573)]) in
  any_of_person for_person st q584
let clue06 st q567 =
  let for_neighbors_pair per1 per2 q545 =
    fresh (c1 q546 q547 q548 q549 c2 q551 q552 q553 q554 q555 q556) (
      per1 === (person c1 q546 q547 q548 q549)) (per2 === (person c2 q551 q552 q553 q554))
      (conde [(c1 === (ivory ())) &&& (q555 === (!! true)); (q555 === (!! false)) &&& (c1 =/= (ivory ()))])
      (conde [(c2 === (green ())) &&& (q556 === (!! true)); (q556 === (!! false)) &&& (c2 =/= (green ()))])
      (conde [(q555 === (!! false)) &&& (q545 === (!! false)); (q555 === (!! true)) &&& (q545 === q556)]) in
  any_of_neighbors_pair for_neighbors_pair st q567
let clue07 st q544 =
  let for_person per q528 =
    fresh (q529 q530 q531 s p q532 q533) (per === (person q529 q530 q531 s p))
      (conde [(s === (old_Gold ())) &&& (q532 === (!! true)); (q532 === (!! false)) &&& (s =/= (old_Gold ()))])
      (conde [(p === (snails ())) &&& (q533 === (!! true)); (q533 === (!! false)) &&& (p =/= (snails ()))])
      (conde [(q532 === (!! false)) &&& (q528 === (!! false)); (q532 === (!! true)) &&& (q528 === q533)]) in
  any_of_person for_person st q544
let clue08 st q527 =
  let for_person per q511 =
    fresh (c q512 q513 s q514 q515 q516) (per === (person c q512 q513 s q514))
      (conde [(c === (yellow ())) &&& (q515 === (!! true)); (q515 === (!! false)) &&& (c =/= (yellow ()))])
      (conde [(s === (kools ())) &&& (q516 === (!! true)); (q516 === (!! false)) &&& (s =/= (kools ()))])
      (conde [(q515 === (!! false)) &&& (q511 === (!! false)); (q515 === (!! true)) &&& (q511 === q516)]) in
  any_of_person for_person st q527
let clue09 st q499 =
  fresh (q500 q501 q502 q503 d q504 q505 q506 q507) (st === (state q500 q501 (person q502 q503 d q504 q505) q506 q507))
    (conde [(d === (milk ())) &&& (q499 === (!! true)); (q499 === (!! false)) &&& (d =/= (milk ()))])
let clue10 st q487 =
  fresh (q488 n q489 q490 q491 q492 q493 q494 q495) (st === (state (person q488 n q489 q490 q491) q492 q493 q494 q495))
    (conde [(n === (norwegian ())) &&& (q487 === (!! true)); (q487 === (!! false)) &&& (n =/= (norwegian ()))])
let clue11 st q486 =
  let for_neighbors_pair per1 per2 q448 =
    fresh (q449 q450 q451 s1 p1 q453 q454 q455 s2 p2 q456 q457 q462 q463 q474 q475) (
      per1 === (person q449 q450 q451 s1 p1)) (per2 === (person q453 q454 q455 s2 p2))
      (conde [(s1 === (chesterfield ())) &&& (q462 === (!! true)); (q462 === (!! false)) &&& (s1 =/= (chesterfield ()))])
      (conde [(p2 === (fox ())) &&& (q463 === (!! true)); (q463 === (!! false)) &&& (p2 =/= (fox ()))])
      (conde [(q462 === (!! false)) &&& (q456 === (!! false)); (q462 === (!! true)) &&& (q456 === q463)])
      (conde [(p1 === (fox ())) &&& (q474 === (!! true)); (q474 === (!! false)) &&& (p1 =/= (fox ()))])
      (conde [(s2 === (chesterfield ())) &&& (q475 === (!! true)); (q475 === (!! false)) &&& (s2 =/= (chesterfield ()))])
      (conde [(q474 === (!! false)) &&& (q457 === (!! false)); (q474 === (!! true)) &&& (q457 === q475)])
      (conde [(q456 === (!! true)) &&& (q448 === (!! true)); (q456 === (!! false)) &&& (q448 === q457)]) in
  any_of_neighbors_pair for_neighbors_pair st q486
let clue12 st q447 =
  let for_neighbors_pair per1 per2 q409 =
    fresh (q410 q411 q412 s1 p1 q414 q415 q416 s2 p2 q417 q418 q423 q424 q435 q436) (
      per1 === (person q410 q411 q412 s1 p1)) (per2 === (person q414 q415 q416 s2 p2))
      (conde [(s1 === (kools ())) &&& (q423 === (!! true)); (q423 === (!! false)) &&& (s1 =/= (kools ()))])
      (conde [(p2 === (hourse ())) &&& (q424 === (!! true)); (q424 === (!! false)) &&& (p2 =/= (hourse ()))])
      (conde [(q423 === (!! false)) &&& (q417 === (!! false)); (q423 === (!! true)) &&& (q417 === q424)])
      (conde [(p1 === (hourse ())) &&& (q435 === (!! true)); (q435 === (!! false)) &&& (p1 =/= (hourse ()))])
      (conde [(s2 === (kools ())) &&& (q436 === (!! true)); (q436 === (!! false)) &&& (s2 =/= (kools ()))])
      (conde [(q435 === (!! false)) &&& (q418 === (!! false)); (q435 === (!! true)) &&& (q418 === q436)])
      (conde [(q417 === (!! true)) &&& (q409 === (!! true)); (q417 === (!! false)) &&& (q409 === q418)]) in
  any_of_neighbors_pair for_neighbors_pair st q447
let clue13 st q408 =
  let for_person p q392 =
    fresh (q393 q394 d s q395 q396 q397) (p === (person q393 q394 d s q395))
      (conde [(s === (lacky_Strike ())) &&& (q396 === (!! true)); (q396 === (!! false)) &&& (s =/= (lacky_Strike ()))])
      (conde [(d === (orange_juice ())) &&& (q397 === (!! true)); (q397 === (!! false)) &&& (d =/= (orange_juice ()))])
      (conde [(q396 === (!! false)) &&& (q392 === (!! false)); (q396 === (!! true)) &&& (q392 === q397)]) in
  any_of_person for_person st q408
let clue14 st q391 =
  let for_person per q375 =
    fresh (q376 n q377 s q378 q379 q380) (per === (person q376 n q377 s q378))
      (conde [(n === (japanese ())) &&& (q379 === (!! true)); (q379 === (!! false)) &&& (n =/= (japanese ()))])
      (conde [(s === (parliament ())) &&& (q380 === (!! true)); (q380 === (!! false)) &&& (s =/= (parliament ()))])
      (conde [(q379 === (!! false)) &&& (q375 === (!! false)); (q379 === (!! true)) &&& (q375 === q380)]) in
  any_of_person for_person st q391
let clue15 st q374 =
  let for_neighbors_pair per1 per2 q336 =
    fresh (c1 n1 q337 q338 q339 c2 n2 q341 q342 q343 q344 q345 q350 q351 q362 q363) (
      per1 === (person c1 n1 q337 q338 q339)) (per2 === (person c2 n2 q341 q342 q343))
      (conde [(n1 === (norwegian ())) &&& (q350 === (!! true)); (q350 === (!! false)) &&& (n1 =/= (norwegian ()))])
      (conde [(c2 === (blue ())) &&& (q351 === (!! true)); (q351 === (!! false)) &&& (c2 =/= (blue ()))])
      (conde [(q350 === (!! false)) &&& (q344 === (!! false)); (q350 === (!! true)) &&& (q344 === q351)])
      (conde [(c1 === (blue ())) &&& (q362 === (!! true)); (q362 === (!! false)) &&& (c1 =/= (blue ()))])
      (conde [(n2 === (norwegian ())) &&& (q363 === (!! true)); (q363 === (!! false)) &&& (n2 =/= (norwegian ()))])
      (conde [(q362 === (!! false)) &&& (q345 === (!! false)); (q362 === (!! true)) &&& (q345 === q363)])
      (conde [(q344 === (!! true)) &&& (q336 === (!! true)); (q344 === (!! false)) &&& (q336 === q345)]) in
  any_of_neighbors_pair for_neighbors_pair st q374
let all_present st q335 =
  let for_person per q90 =
    fresh
      (c n d s p q91 q92 q97 q98 q106 q107 q115 q116 q124 q125 q136 q137 q142 q143 q151 q152 q160 q161 q169 q170 q181 q182 q187 q188 q196 q197 q205 q206 q214 q215 q226 q227 q232
         q233 q241 q242 q250 q251 q259 q260 q271 q272 q280 q281 q289 q290 q298 q299) (
      per === (person c n d s p)) (conde [(c === (yellow ())) &&& (q97 === (!! true)); (q97 === (!! false)) &&& (c =/= (yellow ()))])
      (conde [(c === (blue ())) &&& (q106 === (!! true)); (q106 === (!! false)) &&& (c =/= (blue ()))])
      (conde [(c === (red ())) &&& (q115 === (!! true)); (q115 === (!! false)) &&& (c =/= (red ()))])
      (conde [(c === (ivory ())) &&& (q124 === (!! true)); (q124 === (!! false)) &&& (c =/= (ivory ()))])
      (conde [(c === (green ())) &&& (q125 === (!! true)); (q125 === (!! false)) &&& (c =/= (green ()))])
      (conde [(q124 === (!! true)) &&& (q116 === (!! true)); (q124 === (!! false)) &&& (q116 === q125)])
      (conde [(q115 === (!! true)) &&& (q107 === (!! true)); (q115 === (!! false)) &&& (q107 === q116)])
      (conde [(q106 === (!! true)) &&& (q98 === (!! true)); (q106 === (!! false)) &&& (q98 === q107)])
      (conde [(q97 === (!! true)) &&& (q91 === (!! true)); (q97 === (!! false)) &&& (q91 === q98)])
      (conde [(n === (norwegian ())) &&& (q142 === (!! true)); (q142 === (!! false)) &&& (n =/= (norwegian ()))])
      (conde [(n === (ukrainian ())) &&& (q151 === (!! true)); (q151 === (!! false)) &&& (n =/= (ukrainian ()))])
      (conde [(n === (englishman ())) &&& (q160 === (!! true)); (q160 === (!! false)) &&& (n =/= (englishman ()))])
      (conde [(n === (spaniard ())) &&& (q169 === (!! true)); (q169 === (!! false)) &&& (n =/= (spaniard ()))])
      (conde [(n === (japanese ())) &&& (q170 === (!! true)); (q170 === (!! false)) &&& (n =/= (japanese ()))])
      (conde [(q169 === (!! true)) &&& (q161 === (!! true)); (q169 === (!! false)) &&& (q161 === q170)])
      (conde [(q160 === (!! true)) &&& (q152 === (!! true)); (q160 === (!! false)) &&& (q152 === q161)])
      (conde [(q151 === (!! true)) &&& (q143 === (!! true)); (q151 === (!! false)) &&& (q143 === q152)])
      (conde [(q142 === (!! true)) &&& (q136 === (!! true)); (q142 === (!! false)) &&& (q136 === q143)])
      (conde [(d === (water ())) &&& (q187 === (!! true)); (q187 === (!! false)) &&& (d =/= (water ()))])
      (conde [(d === (tea ())) &&& (q196 === (!! true)); (q196 === (!! false)) &&& (d =/= (tea ()))])
      (conde [(d === (milk ())) &&& (q205 === (!! true)); (q205 === (!! false)) &&& (d =/= (milk ()))])
      (conde [(d === (orange_juice ())) &&& (q214 === (!! true)); (q214 === (!! false)) &&& (d =/= (orange_juice ()))])
      (conde [(d === (coffee ())) &&& (q215 === (!! true)); (q215 === (!! false)) &&& (d =/= (coffee ()))])
      (conde [(q214 === (!! true)) &&& (q206 === (!! true)); (q214 === (!! false)) &&& (q206 === q215)])
      (conde [(q205 === (!! true)) &&& (q197 === (!! true)); (q205 === (!! false)) &&& (q197 === q206)])
      (conde [(q196 === (!! true)) &&& (q188 === (!! true)); (q196 === (!! false)) &&& (q188 === q197)])
      (conde [(q187 === (!! true)) &&& (q181 === (!! true)); (q187 === (!! false)) &&& (q181 === q188)])
      (conde [(s === (kools ())) &&& (q232 === (!! true)); (q232 === (!! false)) &&& (s =/= (kools ()))])
      (conde [(s === (chesterfield ())) &&& (q241 === (!! true)); (q241 === (!! false)) &&& (s =/= (chesterfield ()))])
      (conde [(s === (old_Gold ())) &&& (q250 === (!! true)); (q250 === (!! false)) &&& (s =/= (old_Gold ()))])
      (conde [(s === (lacky_Strike ())) &&& (q259 === (!! true)); (q259 === (!! false)) &&& (s =/= (lacky_Strike ()))])
      (conde [(s === (parliament ())) &&& (q260 === (!! true)); (q260 === (!! false)) &&& (s =/= (parliament ()))])
      (conde [(q259 === (!! true)) &&& (q251 === (!! true)); (q259 === (!! false)) &&& (q251 === q260)])
      (conde [(q250 === (!! true)) &&& (q242 === (!! true)); (q250 === (!! false)) &&& (q242 === q251)])
      (conde [(q241 === (!! true)) &&& (q233 === (!! true)); (q241 === (!! false)) &&& (q233 === q242)])
      (conde [(q232 === (!! true)) &&& (q226 === (!! true)); (q232 === (!! false)) &&& (q226 === q233)])
      (conde [(p === (fox ())) &&& (q271 === (!! true)); (q271 === (!! false)) &&& (p =/= (fox ()))])
      (conde [(p === (hourse ())) &&& (q280 === (!! true)); (q280 === (!! false)) &&& (p =/= (hourse ()))])
      (conde [(p === (snails ())) &&& (q289 === (!! true)); (q289 === (!! false)) &&& (p =/= (snails ()))])
      (conde [(p === (dog ())) &&& (q298 === (!! true)); (q298 === (!! false)) &&& (p =/= (dog ()))])
      (conde [(p === (zebra ())) &&& (q299 === (!! true)); (q299 === (!! false)) &&& (p =/= (zebra ()))])
      (conde [(q298 === (!! true)) &&& (q290 === (!! true)); (q298 === (!! false)) &&& (q290 === q299)])
      (conde [(q289 === (!! true)) &&& (q281 === (!! true)); (q289 === (!! false)) &&& (q281 === q290)])
      (conde [(q280 === (!! true)) &&& (q272 === (!! true)); (q280 === (!! false)) &&& (q272 === q281)])
      (conde [(q271 === (!! true)) &&& (q227 === (!! true)); (q271 === (!! false)) &&& (q227 === q272)])
      (conde [(q226 === (!! false)) &&& (q182 === (!! false)); (q226 === (!! true)) &&& (q182 === q227)])
      (conde [(q181 === (!! false)) &&& (q137 === (!! false)); (q181 === (!! true)) &&& (q137 === q182)])
      (conde [(q136 === (!! false)) &&& (q92 === (!! false)); (q136 === (!! true)) &&& (q92 === q137)])
      (conde [(q91 === (!! false)) &&& (q90 === (!! false)); (q91 === (!! true)) &&& (q90 === q92)]) in
  fresh (p1 p2 p3 p4 p5 q311 q312 q317 q318 q323 q324 q329 q330) (st === (state p1 p2 p3 p4 p5)) (
    for_person p1 q311) (for_person p2 q317) (for_person p3 q323) (for_person p4 q329) (
    for_person p5 q330) (conde [(q329 === (!! false)) &&& (q324 === (!! false)); (q329 === (!! true)) &&& (q324 === q330)])
    (conde [(q323 === (!! false)) &&& (q318 === (!! false)); (q323 === (!! true)) &&& (q318 === q324)])
    (conde [(q317 === (!! false)) &&& (q312 === (!! false)); (q317 === (!! true)) &&& (q312 === q318)])
    (conde [(q311 === (!! false)) &&& (q335 === (!! false)); (q311 === (!! true)) &&& (q335 === q312)])
let check_state st q2 =
  fresh (q0 q1 q6 q7 q12 q13 q18 q19 q24 q25 q30 q31 q36 q37 q42 q43 q48 q49 q54 q55 q60 q61 q66 q67 q72 q73 q78 q79 q84 q85) 
        (all_different st q0) 
    (clue02 st q6) 
    (clue03 st q12) 
    (clue04 st q18) 
    (clue05 st q24) (clue06 st q30) (clue07 st q36) (clue08 st q42) (clue09 st q48) (
    clue10 st q54) (clue11 st q60) (clue12 st q66) (clue13 st q72) (clue14 st q78) (
    clue15 st q84) (all_present st q85) 
    (conde [(q84 === (!! false)) &&& (q79 === (!! false)); (q84 === (!! true)) &&& (q79 === q85)])
    (conde [(q78 === (!! false)) &&& (q73 === (!! false)); (q78 === (!! true)) &&& (q73 === q79)])
    (conde [(q72 === (!! false)) &&& (q67 === (!! false)); (q72 === (!! true)) &&& (q67 === q73)])
    (conde [(q66 === (!! false)) &&& (q61 === (!! false)); (q66 === (!! true)) &&& (q61 === q67)])
    (conde [(q60 === (!! false)) &&& (q55 === (!! false)); (q60 === (!! true)) &&& (q55 === q61)])
    (conde [(q54 === (!! false)) &&& (q49 === (!! false)); (q54 === (!! true)) &&& (q49 === q55)])
    (conde [(q48 === (!! false)) &&& (q43 === (!! false)); (q48 === (!! true)) &&& (q43 === q49)])
    (conde [(q42 === (!! false)) &&& (q37 === (!! false)); (q42 === (!! true)) &&& (q37 === q43)])
    (conde [(q36 === (!! false)) &&& (q31 === (!! false)); (q36 === (!! true)) &&& (q31 === q37)])
    (conde [(q30 === (!! false)) &&& (q25 === (!! false)); (q30 === (!! true)) &&& (q25 === q31)])
    (conde [(q24 === (!! false)) &&& (q19 === (!! false)); (q24 === (!! true)) &&& (q19 === q25)])
    (conde [(q18 === (!! false)) &&& (q13 === (!! false)); (q18 === (!! true)) &&& (q13 === q19)])
    (conde [(q12 === (!! false)) &&& (q7 === (!! false)); (q12 === (!! true)) &&& (q7 === q13)])
    (conde [(q6 === (!! false)) &&& (q1 === (!! false)); (q6 === (!! true)) &&& (q1 === q7)])
    (conde [(q0 === (!! false)) &&& (q2 === (!! false)); (q0 === (!! true)) &&& (q2 === q1)])


(*************************************************)

let show_hause_color = function
  | Yellow       -> "Yellow      "
  | Blue         -> "Blue        "
  | Red          -> "Red         "
  | Ivory        -> "Ivory       "
  | Green        -> "Green       "

let show_nationality = function
  | Norwegian    -> "Norwegian   "
  | Ukrainian    -> "Ukrainian   "
  | Englishman   -> "Englishman  "
  | Spaniard     -> "Spaniard    "
  | Japanese     -> "Japanese    "

let show_drink = function
  | Water        -> "Water       "
  | Tea          -> "Tea         "
  | Milk         -> "Milk        "
  | Orange_juice -> "Orange_juice"
  | Coffee       -> "Coffee      "

let show_smoke = function
  | Kools        -> "Kools       "
  | Chesterfield -> "Chesterfield"
  | Old_Gold     -> "Old_Gold    "
  | Lacky_Strike -> "Lacky_Strike"
  | Parliament   -> "Parliament  "

let show_pet = function
  | Fox          -> "Fox         "
  | Hourse       -> "Hourse      "
  | Snails       -> "Snails      "
  | Dog          -> "Dog         "
  | Zebra        -> "Zebra       "

let show_person = function
  | Person (c, n, d, s, p) -> Printf.sprintf "%s + %s + %s + %s + %s" (show_hause_color c) (show_nationality n) (show_drink d) (show_smoke s) (show_pet p)

let show_state = function
  | State (p1, p2, p3, p4, p5) -> Printf.sprintf "\n  %s\n  %s\n  %s\n  %s\n  %s\n" (show_person p1) (show_person p2) (show_person p3) (show_person p4) (show_person p5)

let myshow = show_state

(*************************************************)

(** For high order conversion **)
(* let check_state q r = check_state ((===) q) r *)

let _ =
  run_exn myshow (1) q qh ("answers", fun q -> deepen (10000000) @@ check_state q !!true)
