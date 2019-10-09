open GT
open OCanren
open OCanren.Std
open Tester


let conj = (<&>)
let (&&&) = (<&>)

type 'a0 gnat =
  | O 
  | S of 'a0 
module For_gnat = (Fmap)(struct let rec fmap fa0 = function | O -> O | S a0 -> S (fa0 a0)
                                type 'a0 t = 'a0 gnat end)
let rec o () = inj (For_gnat.distrib O)
and s x__0 = inj (For_gnat.distrib (S x__0))
type 'a0 gstep =
  | Left of 'a0 
  | Right of 'a0 
  | Fill 
  | Pour of 'a0 
module For_gstep =
  (Fmap)(struct let rec fmap fa0 = function | Left a0 -> Left (fa0 a0) | Right a0 -> Right (fa0 a0) | Fill -> Fill | Pour a0 -> Pour (fa0 a0)
                type 'a0 t = 'a0 gstep end)
let rec left x__0 = inj (For_gstep.distrib (Left x__0))
and right x__0 = inj (For_gstep.distrib (Right x__0))
and fill () = inj (For_gstep.distrib Fill)
and pour x__0 = inj (For_gstep.distrib (Pour x__0))
type ('a1, 'a0) gstate =
  | St of 'a1 * 'a1 * 'a0 
module For_gstate = (Fmap2)(struct let rec fmap fa1 fa0 = function | St (a1_0, a1_1, a0_2) -> St ((fa1 a1_0), (fa1 a1_1), (fa0 a0_2))
                                   type ('a1, 'a0) t = ('a1, 'a0) gstate end)
let rec st x__0 x__1 x__2 = inj (For_gstate.distrib (St (x__0, x__1, x__2)))
let rec (|+|) a b q170 = ((a === (o ())) &&& (b === q170)) ||| (fresh (x) (a === (s x)) ((|+|) x (s b) q170))
let rec (|>=|) a b q164 =
  ((a === (o ())) &&& (conde [(b === (o ())) &&& (q164 === (!! true)); (q164 === (!! false)) &&& (b =/= (o ()))])) |||
    (fresh (x) (a === (s x)) (((b === (o ())) &&& (q164 === (!! true))) ||| (fresh (y) (b === (s y)) ((|>=|) x y q164))))
let rec (|-|) a b q160 = ((b === (o ())) &&& (a === q160)) ||| (fresh (y) (b === (s y)) (((a === (o ())) &&& (q160 === (o ()))) ||| (fresh (x) (a === (s x)) ((|-|) x y q160))))
let rec elem l n q157 = fresh (x xs) (l === (x % xs)) (((n === (o ())) &&& (x === q157)) ||| (fresh (m) (n === (s m)) (elem xs m q157)))
let rec changeElem l n f q151 =
  fresh (x xs) (l === (x % xs)) ((fresh (q153) (n === (o ())) (q151 === (q153 % xs)) (f x q153)) ||| (fresh (m q155) (n === (s m)) (q151 === (x % q155)) (changeElem xs m f q155)))
let checkStep step state len cop q72 =
  fresh (pos fuel sts) (state === (st pos fuel sts))
    (conde
       [fresh (d q74 q75 q80 q81) (step === (left d)) ((|>=|) pos d q74) (
          (|>=|) fuel d q80) (conde [(d === (o ())) &&& (q81 === (!! false)); (q81 === (!! true)) &&& (d =/= (o ()))])
          (conde [(q80 === (!! false)) &&& (q75 === (!! false)); (q80 === (!! true)) &&& (q75 === q81)])
          (conde [(q74 === (!! false)) &&& (q72 === (!! false)); (q74 === (!! true)) &&& (q72 === q75)]);
       fresh (d q89 q90 q95 q97 q98) (step === (right d)) ((|+|) pos d q95) (
         (|>=|) len q95 q89) ((|>=|) fuel d q97) (conde [(d === (o ())) &&& (q98 === (!! false)); (q98 === (!! true)) &&& (d =/= (o ()))])
         (conde [(q97 === (!! false)) &&& (q90 === (!! false)); (q97 === (!! true)) &&& (q90 === q98)])
         (conde [(q89 === (!! false)) &&& (q72 === (!! false)); (q89 === (!! true)) &&& (q72 === q90)]);
       fresh (f q106 q107 q115 q116 q124 q125) (step === (pour f)) (conde [(pos === len) &&& (q106 === (!! false)); (q106 === (!! true)) &&& (pos =/= len)])
         (conde [(pos === (o ())) &&& (q115 === (!! false)); (q115 === (!! true)) &&& (pos =/= (o ()))])
         (conde [(f === (o ())) &&& (q124 === (!! false)); (q124 === (!! true)) &&& (f =/= (o ()))]) (
         (|>=|) fuel f q125) (conde [(q124 === (!! false)) &&& (q116 === (!! false)); (q124 === (!! true)) &&& (q116 === q125)])
         (conde [(q115 === (!! false)) &&& (q107 === (!! false)); (q115 === (!! true)) &&& (q107 === q116)])
         (conde [(q106 === (!! false)) &&& (q72 === (!! false)); (q106 === (!! true)) &&& (q72 === q107)]);
       (step === (fill ())) &&&
         (((pos === (o ())) &&& (conde [(fuel === cop) &&& (q72 === (!! false)); (q72 === (!! true)) &&& (fuel =/= cop)])) |||
            (fresh (x q137 q138 q146) (pos === (s x)) (conde [(fuel === cop) &&& (q137 === (!! false)); (q137 === (!! true)) &&& (fuel =/= cop)]) (
               elem sts x q146) (conde [(q146 === (o ())) &&& (q138 === (!! false)); (q138 === (!! true)) &&& (q146 =/= (o ()))])
               (conde [(q137 === (!! false)) &&& (q72 === (!! false)); (q137 === (!! true)) &&& (q72 === q138)])))])
let step step state len cop q49 =
  fresh (pos fuel sts) (state === (st pos fuel sts))
    (conde
       [fresh (d q51 q52) (step === (left d)) (q49 === (st q51 q52 sts)) ((|-|) pos d q51) ((|-|) fuel d q52);
       fresh (d q54 q55) (step === (right d)) (q49 === (st q54 q55 sts)) ((|+|) pos d q54) ((|-|) fuel d q55);
       fresh (f x q58 q59) (step === (pour f)) (pos === (s x)) (q49 === (st pos q58 q59)) ((|-|) fuel f q58) (changeElem sts x (fun e -> f |+| e) q59);
       (step === (fill ())) &&&
         (((pos === (o ())) &&& (q49 === (st pos cop sts))) |||
            (fresh (x stationFuel totalFuel q66) (pos === (s x)) (elem sts x stationFuel) (
               (|+|) fuel stationFuel totalFuel) ((|>=|) totalFuel cop q66)
               (conde
                  [fresh (q67) (q66 === (!! true)) (q49 === (st pos cop q67)) (changeElem sts x (fun e -> totalFuel |-| cop) q67);
                  fresh (q69) (q66 === (!! false)) (q49 === (st pos totalFuel q69)) (changeElem sts x (fun e -> fun q71 -> q71 === (o ())) q69)])))])
let isFinishState state len q45 = fresh (pos fuel sts) (state === (st pos fuel sts)) (conde [(pos === len) &&& (q45 === (!! true)); (q45 === (!! false)) &&& (pos =/= len)])
let getFuel step state cop q38 =
  conde
    [fresh (d) (step === (left d)) (q38 === (o ()));
    fresh (d) (step === (right d)) (q38 === (o ()));
    fresh (f) (step === (pour f)) (q38 === (o ()));
    fresh (pos fuel sts) (step === (fill ())) (state === (st pos fuel sts)) (((pos === (o ())) &&& ((|-|) cop fuel q38)) ||| (fresh (x) (pos === (s x)) (q38 === (o ()))))]
let isMove step q33 =
  conde
    [fresh (x) (step === (left x)) (q33 === (!! true));
    fresh (x) (step === (right x)) (q33 === (!! true));
    (step === (fill ())) &&& (q33 === (!! false));
    fresh (x) (step === (pour x)) (q33 === (!! false))]
let checkAnswer answer len cop q32 =
  let rec calcFuel state ans prevIsMove q0 =
    (fresh (q2) (ans === (nil ())) (isFinishState state len q2) (conde [(q2 === (!! true)) &&& (q0 === (some cop)); (q2 === (!! false)) &&& (q0 === (none ()))])) |||
      (fresh (x xs currIsMove q7) (ans === (x % xs)) (isMove x currIsMove)
         (conde [(prevIsMove === currIsMove) &&& (q7 === (!! true)); (q7 === (!! false)) &&& (prevIsMove =/= currIsMove)])
         (conde
            [(q7 === (!! true)) &&& (q0 === (none ()));
            fresh (q10) (q7 === (!! false)) (checkStep x state len cop q10)
              (conde
                 [fresh (q12 q18) 
                    (q10 === (!! true)) 
                    (step x state len cop q18) 
                    (calcFuel q18 xs currIsMove q12)
                    (((q12 === (none ())) &&& (q0 === (none ()))) ||| (fresh (res q14 q16) (q12 === (some res)) (q0 === (some q14)) (getFuel x state cop q16) ((|+|) q16 res q14)))
                    
                    ;
                    (q10 === (!! false)) &&& (q0 === (none ()))])])) in
  fresh (startState)
    (let rec stations n q24 = ((n === (o ())) &&& (q24 === (nil ()))) ||| (fresh (m q26) (n === (s m)) (q24 === ((o ()) % q26)) (stations m q26)) in
     fresh (q28) (startState === (st (o ()) cop q28)) (stations len q28)) (
    calcFuel startState answer (!! false) q32)

(*************************************************)

let show_number num =
  let rec helper = function
  | O   -> 0
  | S x -> 1  + (helper x)
  in
  string_of_int @@ helper num

let show_step = function
  | Left x  -> Printf.sprintf "L%s" @@ show_number x
  | Right x -> Printf.sprintf "R%s" @@ show_number x
  | Fill    -> "F"
  | Pour x  -> Printf.sprintf "P%s" @@ show_number x

let myshow x = show List.ground show_step x

(*************************************************)

let rec of_int i = if i = 0 then o () else s @@ of_int @@ i - 1

(** For high order conversion **)
(* let checkAnswer a q p r = checkAnswer ((===) a) ((===) q) ((===) p) r *)

let () =
  run_exn myshow (1) q qh ("answers", fun q ->
    deepen (1000) @@ checkAnswer q (of_int 8) (of_int 5) (some @@ of_int 20)
  )
