open GT
open MiniKanren
open MiniKanren.Std
open Tester

open Hanoi

let show_stick = function
  | One -> "One"
  | Two -> "Two"
  | Thr -> "Thr"

let show_lstick = show logic show_stick

let show_answer  = show LList.ground @@ show LPair.ground show_stick  show_stick
let lshow_answer = show LList.logic  @@ show LPair.logic  show_lstick show_lstick
let reify_answer x = LList.reify (LPair.reify MiniKanren.reify MiniKanren.reify) x

let run x = runR reify_answer show_answer lshow_answer x

let rec length l n =
  if n = 0 then l === nil () else
  fresh (e l') (l === e % l') (length l' (n - 1))



  let rec less a b =
    fresh (b')
     (b === s b')
     (conde [
       (a === z ());
       fresh (a')
         (a === s a')
         (less a' b')
     ])

  let get name state q39 =
    fresh (s1 s2 s3)
      (state === triple s1 s2 s3)
      (conde [
        (name === one ()) &&& (s1 === q39);
        (name === two ()) &&& (s2 === q39);
        (name === thr ()) &&& (s3 === q39)
      ])

  let set name stack state q34 =
    fresh (s1 s2 s3)
      (state === triple s1 s2 s3)
      (conde [
        (name === one ()) &&& (q34 === triple stack s2 s3);
        (name === two ()) &&& (q34 === triple s1 stack s3);
        (name === thr ()) &&& (q34 === triple s1 s2 stack)
      ])

  let one_step step state q18 =
    fresh (fromN toN q22 x xs q24 q25)
      (step === pair fromN toN)
      (q22 === x % xs)
      (conde [
        (fromN === one ()) &&& (toN === two ());
        (fromN === one ()) &&& (toN === thr ());
        (fromN === two ()) &&& (toN === one ());
        (fromN === two ()) &&& (toN === thr ());
        (fromN === thr ()) &&& (toN === one ());
        (fromN === thr ()) &&& (toN === two ())
      ])
      (get fromN state q22)
      (get toN state q24)
      (conde [
          q24 === nil ();
        fresh (y ys)
          (q24 === y % ys)
          (less x y)
      ])
      (set fromN xs state q25)
      (set toN (x % q24) q25 q18)

  let check = Tabling.tabledrec Tabling.two @@ fun check state steps ->

  conde [
    (steps === nil ()) &&&
    (get (one ()) state (nil ())) &&&
    (get (two ()) state (nil ()));
    fresh (x xs q16)
      (steps === x % xs)
      (one_step x state q16)
      (check q16 xs)
    ]


let rec toN n = if n = 0 then z () else s (toN (n - 1))

let gen_pin n =
  let rec gen_pin m =
    if m = n then nil () else toN m % gen_pin (m + 1) in
  gen_pin 0

let gen n = triple (gen_pin n) (nil ()) (nil ())



let _ =
  run 1 q qh ("first", fun q -> check (gen 5) q &&& length q 31)
