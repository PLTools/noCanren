open GT
open OCanren
open OCanren.Std
open Tester

let conj = (<&>)
let (&&&) = (<&>)

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
let rec greater a0 b0 q114 =
  ((a0 === (o ())) &&& (q114 === (!! false))) ||| (fresh (x) (a0 === (s x)) (((b0 === (o ())) &&& (q114 === (!! true))) ||| (fresh (y) (b0 === (s y)) (greater x y q114))))
let grForPerson x y q97 =
  conde
    [(x === (a ())) &&&
       (conde [(y === (a ())) &&& (q97 === (!! false)); (y === (b ())) &&& (q97 === (!! true)); (y === (c ())) &&& (q97 === (!! true)); (y === (d ())) &&& (q97 === (!! true))]);
    (x === (b ())) &&&
      (conde [(y === (a ())) &&& (q97 === (!! false)); (y === (b ())) &&& (q97 === (!! false)); (y === (c ())) &&& (q97 === (!! false)); (y === (d ())) &&& (q97 === (!! true))]);
    (x === (c ())) &&&
      (conde [(y === (a ())) &&& (q97 === (!! false)); (y === (b ())) &&& (q97 === (!! false)); (y === (c ())) &&& (q97 === (!! false)); (y === (d ())) &&& (q97 === (!! true))]);
    (x === (d ())) &&& (q97 === (!! false))]
let max a0 b0 q93 = fresh (q94) (greater a0 b0 q94) (conde [(q94 === (!! true)) &&& (a0 === q93); (q94 === (!! false)) &&& (b0 === q93)])
let rec add a0 b0 q91 = ((a0 === (o ())) &&& (b0 === q91)) ||| (fresh (x) (a0 === (s x)) (add x (s b0) q91))
let checkPerson state person q77 =
  fresh (l a0 b0 c0 d0) (state === (st l a0 b0 c0 d0))
    (conde
       [(person === (a ())) &&& (conde [(a0 === l) &&& (q77 === (!! true)); (q77 === (!! false)) &&& (a0 =/= l)]);
       (person === (b ())) &&& (conde [(b0 === l) &&& (q77 === (!! true)); (q77 === (!! false)) &&& (b0 =/= l)]);
       (person === (c ())) &&& (conde [(c0 === l) &&& (q77 === (!! true)); (q77 === (!! false)) &&& (c0 =/= l)]);
       (person === (d ())) &&& (conde [(d0 === l) &&& (q77 === (!! true)); (q77 === (!! false)) &&& (d0 =/= l)])])
let checkStep state step q64 =
  (fresh (p) (step === (one p)) (checkPerson state p q64)) |||
    (fresh (p q q65 q66 q71 q72) (step === (two p q)) (checkPerson state p q65) (
       checkPerson state q q71) (grForPerson p q q72) (conde [(q71 === (!! false)) &&& (q66 === (!! false)); (q71 === (!! true)) &&& (q66 === q72)])
       (conde [(q65 === (!! false)) &&& (q64 === (!! false)); (q65 === (!! true)) &&& (q64 === q66)]))
let moveLight state q59 =
  fresh (l a0 b0 c0 d0 q60) (state === (st l a0 b0 c0 d0)) (q59 === (st q60 a0 b0 c0 d0))
    (conde [(l === (!! true)) &&& (q60 === (!! false)); (l === (!! false)) &&& (q60 === (!! true))])
let movePerson state person q41 =
  fresh (l a0 b0 c0 d0) (state === (st l a0 b0 c0 d0))
    (conde
       [fresh (q43) (person === (a ())) (q41 === (st l q43 b0 c0 d0)) (conde [(a0 === (!! true)) &&& (q43 === (!! false)); (a0 === (!! false)) &&& (q43 === (!! true))]);
       fresh (q47) (person === (b ())) (q41 === (st l a0 q47 c0 d0)) (conde [(b0 === (!! true)) &&& (q47 === (!! false)); (b0 === (!! false)) &&& (q47 === (!! true))]);
       fresh (q51) (person === (c ())) (q41 === (st l a0 b0 q51 d0)) (conde [(c0 === (!! true)) &&& (q51 === (!! false)); (c0 === (!! false)) &&& (q51 === (!! true))]);
       fresh (q55) (person === (d ())) (q41 === (st l a0 b0 c0 q55)) (conde [(d0 === (!! true)) &&& (q55 === (!! false)); (d0 === (!! false)) &&& (q55 === (!! true))])])
let step state step q34 =
  (fresh (p q35) (step === (one p)) (movePerson state p q35) (moveLight q35 q34)) |||
    (fresh (p q q37 q39) (step === (two p q)) (movePerson state p q39) (movePerson q39 q q37) (moveLight q37 q34))
let getTime state times q30 = (fresh (p) (state === (one p)) (times p q30)) ||| (fresh (p q q31 q32) (state === (two p q)) (times p q31) (times q q32) (max q31 q32 q30))
let getAnswer answer times q6 =
  fresh (start finish) (start === (st (!! true) (!! true) (!! true) (!! true) (!! true))) (
    finish === (st (!! false) (!! false) (!! false) (!! false) (!! false)))
    
    (let rec getAnswer answer state q9 =
       (fresh (x xs q11) (answer === (x % xs)) (checkStep state x q11)
          (conde
             [fresh (q13 q19) 
                (q11 === (!! true)) 
                (step state x q19) 
                (getAnswer xs q19 q13)
                (((q13 === (none ())) &&& (q9 === (none ()))) ||| (fresh (t1 q15 q17) (q13 === (some t1)) (q9 === (some q15)) (getTime x times q17) (add q17 t1 q15)))
                
                ;
             (q11 === (!! false)) &&& (q9 === (none ()))]))
         |||
         (fresh (q23) (answer === (nil ())) (conde [(state === finish) &&& (q23 === (!! true)); (q23 === (!! false)) &&& (state =/= finish)])
            (conde [(q23 === (!! true)) &&& (q9 === (some (o ()))); (q23 === (!! false)) &&& (q9 === (none ()))])) in
     getAnswer answer start q6)
let standartTimes p q0 =
  conde
    [(p === (a ())) &&& (q0 === (s (o ())));
    (p === (b ())) &&& (q0 === (s (s (o ()))));
    (p === (c ())) &&& (q0 === (s (s (s (s (s (o ())))))));
    (p === (d ())) &&& (q0 === (s (s (s (s (s (s (s (s (s (s (o ()))))))))))))]

(*************************************************)

let show_person = function
 | A -> "A"
 | B -> "B"
 | C -> "C"
 | D -> "D"

let show_step f = function
 | One x     -> f x
 | Two (x,y) -> Printf.sprintf "(%s, %s)" (f x) (f y)

let myshow x = show List.ground (show_step show_person) x

(*************************************************)

let rec int2nat i = if i = 0 then o () else s @@ int2nat @@ i - 1

(** For high order conversion **)
(* let getAnswer q t r = getAnswer ((===) q) t r *)

let _ =
  run_exn myshow (1) q qh ("answers", fun q ->
    deepen (82) @@ getAnswer q standartTimes (int2nat 17 |> some)
  )
