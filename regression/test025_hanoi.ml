open GT
open OCanren
open OCanren.Std
open Tester


let conj = (<&>)
let (&&&) = (<&>)


type 'a0 gnat =
  | Z 
  | S of 'a0 

module For_gnat = (Fmap)(struct let rec fmap fa0 = function | Z -> Z | S a0 -> S (fa0 a0)
                                type 'a0 t = 'a0 gnat end)
let rec z () = inj (For_gnat.distrib Z)
and s x__0 = inj (For_gnat.distrib (S x__0))
type stick =
  | One 
  | Two 
  | Thr 
let one () = !! One
let two () = !! Two
let thr () = !! Thr
type 'a triple =
  | Triple of 'a * 'a * 'a 
module For_triple = (Fmap)(struct let rec fmap fa = function | Triple (a_0, a_1, a_2) -> Triple ((fa a_0), (fa a_1), (fa a_2))
                                  type 'a t = 'a triple end)
let rec triple x__0 x__1 x__2 = inj (For_triple.distrib (Triple (x__0, x__1, x__2)))


let rec less a b = 
  fresh (b') 
    (b === s b') 
    (conde [
      (a === z ()); 
      fresh (a') 
        (a === s a') 
        (less a' b')
     ])

let get name state q43 =
  fresh (s1 s2 s3) 
    (state === triple s1 s2 s3) 
    (conde [
      (name === one ()) &&& (s1 === q43); 
      (name === two ()) &&& (s2 === q43); 
      (name === thr ()) &&& (s3 === q43)
    ])


let set name stack state q38 =
  fresh (s1 s2 s3) 
    (state === triple s1 s2 s3)
    (conde [
      (name === one ()) &&& (q38 === triple stack s2 s3); 
      (name === two ()) &&& (q38 === triple s1 stack s3); 
      (name === thr ()) &&& (q38 === triple s1 s2 stack)
    ])

let one_step step state q22 =
  fresh (fromN toN x xs q28) 
    (step === pair fromN toN) 
    (fromN =/= toN) 
    (get fromN state (x % xs)) 
    (get toN state q28)
    (conde [
      fresh (q29) 
        (q28 === nil ()) 
        (set fromN xs state q29) 
        (set toN (x % (nil ())) q29 q22);
      fresh (y ys q33) 
        (q28 === y % ys) 
        (less x y) 
        (set fromN xs state q33) 
        (set toN (x % (y % ys)) q33 q22)
    ])


let rec check state steps = conde [
    (steps === nil ()) &&& (get (one ()) state (nil ())) &&& (get (two ()) state (nil ()));
    fresh (x xs q20) 
     (steps === (x % xs)) 
     (one_step x state q20) 
     (check q20 xs)]

(*************************************************)

let show_stick = function
  | One -> "One"
  | Two -> "Two"
  | Thr -> "Thr"

let show_lstick = show logic show_stick

let show_answer  = show LList.ground @@ show LPair.ground show_stick  show_stick
let lshow_answer = show LList.logic  @@ show LPair.logic  show_lstick show_lstick
let reify_answer x = LList.reify (LPair.reify OCanren.reify OCanren.reify) x

let run x = runR reify_answer show_answer lshow_answer x

(*************************************************)

let rec toN n = if n = 0 then z () else s (toN (n - 1))

let gen_pin n =
  let rec gen_pin m =
    if m = n then nil () else toN m % gen_pin (m + 1) in
  gen_pin 0

let gen n = triple (gen_pin n) (nil ()) (nil ())

(** For high order conversion **)
(* let check p q r = check ((===) p) ((===) q) r *)

let _ =
  run 1 q qh ("first", fun q ->
    deepen (104) @@ check (gen 3) q
  )
