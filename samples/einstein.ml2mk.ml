type hause_color =
  | Yellow
  | Blue
  | Red
  | Ivory
  | Green

type nationality =
  | Norwegian
  | Ukrainian
  | Englishman
  | Spaniard
  | Japanese

type drink =
  | Water
  | Tea
  | Milk
  | Orange_juice
  | Coffee

type smoke =
  | Kools
  | Chesterfield
  | Old_Gold
  | Lacky_Strike
  | Parliament

type pet =
  | Fox
  | Hourse
  | Snails
  | Dog
  | Zebra

(* Все характеристики уникальны *)
let all_different (p1, p2, p3, p4, p5) =
  let two_different a b =
    match a with
    | c1, n1, d1, s1, p1 ->
      (match b with
       | c2, n2, d2, s2, p2 -> c1 <> c2 && n1 <> n2 && d1 <> d2 && s1 <> s2 && p1 <> p2)
  in
  two_different p1 p2
  && two_different p1 p3
  && two_different p1 p4
  && two_different p1 p5
  && two_different p2 p3
  && two_different p2 p4
  && two_different p2 p5
  && two_different p3 p4
  && two_different p3 p5
  && two_different p4 p5
;;

let any_of_person f (p1, p2, p3, p4, p5) = f p1 || f p2 || f p3 || f p4 || f p5

let any_of_neighbors_pair f (p1, p2, p3, p4, p5) =
  f p1 p2 || f p2 p3 || f p3 p4 || f p4 p5
;;

(* Подсказка 1: На улице стоят пять домов. *)

(* Англичанин живёт в красном доме *)
let clue02 st =
  let for_person (c, n, _, _, _) = n = Englishman && c = Red in
  any_of_person for_person st
;;

(* У испанца есть собака *)
let clue03 st =
  let for_person (_, n, _, _, p) = n = Spaniard && p = Dog in
  any_of_person for_person st
;;

(* В зелёном доме пьют кофе *)
let clue04 st =
  let for_person (c, _, d, _, _) = c = Green && d = Coffee in
  any_of_person for_person st
;;

(* Украинец пьёт чай *)
let clue05 st =
  let for_person (_, n, d, _, _) = n = Ukrainian && d = Tea in
  any_of_person for_person st
;;

(* Зелёный дом стоит сразу справа от белого дома *)
let clue06 st =
  let for_neighbors_pair (c1, _, _, _, _) (c2, _, _, _, _) = c1 = Ivory && c2 = Green in
  any_of_neighbors_pair for_neighbors_pair st
;;

(* Тот, кто курит Old Gold, разводит улиток *)
let clue07 st =
  let for_person (_, _, _, s, p) = s = Old_Gold && p = Snails in
  any_of_person for_person st
;;

(* В жёлтом доме курят Kool *)
let clue08 st =
  let for_person (c, _, _, s, _) = c = Yellow && s = Kools in
  any_of_person for_person st
;;

(* В центральном доме пьют молоко *)
let clue09 (_, _, (_, _, d, _, _), _, _) = d = Milk

(* Норвежец живёт в первом доме *)
let clue10 ((_, n, _, _, _), _, _, _, _) = n = Norwegian

(* Сосед того, кто курит Chesterfield, держит лису *)
let clue11 st =
  let for_neighbors_pair (_, _, _, s1, p1) (_, _, _, s2, p2) =
    (s1 = Chesterfield && p2 = Fox) || (p1 = Fox && s2 = Chesterfield)
  in
  any_of_neighbors_pair for_neighbors_pair st
;;

(* В доме по соседству с тем, в котором держат лошадь, курят Kool *)
let clue12 st =
  let for_neighbors_pair (_, _, _, s1, p1) (_, _, _, s2, p2) =
    (s1 = Kools && p2 = Hourse) || (p1 = Hourse && s2 = Kools)
  in
  any_of_neighbors_pair for_neighbors_pair st
;;

(* Тот, кто курит Lucky Strike, пьёт апельсиновый сок *)
let clue13 st =
  let for_person (_, _, d, s, _) = s = Lacky_Strike && d = Orange_juice in
  any_of_person for_person st
;;

(* Японец курит Parliament *)
let clue14 st =
  let for_person (_, n, _, s, _) = n = Japanese && s = Parliament in
  any_of_person for_person st
;;

(* Норвежец живёт рядом с синим домом *)
let clue15 st =
  let for_neighbors_pair (c1, n1, _, _, _) (c2, n2, _, _, _) =
    (n1 = Norwegian && c2 = Blue) || (c1 = Blue && n2 = Norwegian)
  in
  any_of_neighbors_pair for_neighbors_pair st
;;

(* Все характеристики присутствуют *)
let all_present (p1, p2, p3, p4, p5) =
  let for_person (c, n, d, s, p) =
    (c = Yellow || c = Blue || c = Red || c = Ivory || c = Green)
    && (n = Norwegian || n = Ukrainian || n = Englishman || n = Spaniard || n = Japanese)
    && (d = Water || d = Tea || d = Milk || d = Orange_juice || d = Coffee)
    && (s = Kools
       || s = Chesterfield
       || s = Old_Gold
       || s = Lacky_Strike
       || s = Parliament)
    && (p = Fox || p = Hourse || p = Snails || p = Dog || p = Zebra)
  in
  for_person p1 && for_person p2 && for_person p3 && for_person p4 && for_person p5
;;

let check_state st =
  all_different st
  && clue02 st
  && clue03 st
  && clue04 st
  && clue05 st
  && clue06 st
  && clue07 st
  && clue08 st
  && clue09 st
  && clue10 st
  && clue11 st
  && clue12 st
  && clue13 st
  && clue14 st
  && clue15 st
  && all_present st
;;

(*
let answer = (
  (Yellow, Norwegian , Water       , Kools       , Fox   ),
  (Blue  , Ukrainian , Tea         , Chesterfield, Hourse),
  (Red   , Englishman, Milk        , Old_Gold    , Snails),
  (Ivory , Spaniard  , Orange_juice, Lacky_Strike, Dog   ),
  (Green , Japanese  , Coffee      , Parliament  , Zebra ))
*)
