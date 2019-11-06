open GT
open OCanren
open OCanren.Std
open Tester


let call_fresh = call_fresh'
let conj = conj'
let delay = delay'


let (===) = (===!)
let (=/=) = (=/=!)
let (&&&) = (&&&!) 50
let conde = conde'

(******************************************)
;;

@type 'tree tree = Leaf | Node of 'tree * 'tree with show

module For_tree = (Fmap)
  (struct
    let rec fmap fa = function
      | Leaf       -> Leaf
      | Node (a1, a2) -> Node (fa a1, fa a2)
    type 'a t = 'a tree
  end)

let leaf ()    = inj @@ For_tree.distrib @@ Leaf
let node a1 a2 = inj @@ For_tree.distrib @@ Node (a1, a2)

let rec ltree t =
  conde [
    t === leaf ();
    call_fresh @@ fun t' ->
      (t === node t' (leaf ()) &&& ltree t')]

let rec rtree t =
  conde [
    t === leaf ();
    call_fresh @@ fun t' ->
      (t === node (leaf ()) t' &&& rtree t')]

let main_goal q = ltree q &&& rtree q


let rec tree_reify x = For_tree.reify tree_reify x
let rec show_tree  x = show tree show_tree x
let rec show_ltree x = show logic (show tree show_ltree) x

let run x = runR tree_reify show_tree show_ltree x

let () =
  run (-1) q qh ("tree", fun q -> fair_transform @@ main_goal q);;

(******************************************)

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
  call_fresh @@ fun b' -> 
    (b === s b') &&&
    (conde [
      (a === z ()); 
      call_fresh @@ fun a' -> 
        (a === s a') &&&
        (less a' b')
     ])

let get name state q43 =
  call_fresh @@ fun s1 -> call_fresh @@ fun s2 -> call_fresh @@ fun s3 -> 
    (state === triple s1 s2 s3) &&&
    (conde [
      (name === one ()) &&& (s1 === q43); 
      (name === two ()) &&& (s2 === q43); 
      (name === thr ()) &&& (s3 === q43)
    ])


let set name stack state q38 =
  call_fresh @@ fun s1 -> call_fresh @@ fun s2 -> call_fresh @@ fun s3 -> 
    (state === triple s1 s2 s3) &&&
    (conde [
      (name === one ()) &&& (q38 === triple stack s2 s3); 
      (name === two ()) &&& (q38 === triple s1 stack s3); 
      (name === thr ()) &&& (q38 === triple s1 s2 stack)
    ])




let one_step step state q22 =
  call_fresh @@ fun fromN -> call_fresh @@ fun toN -> call_fresh @@ fun x -> call_fresh @@ fun xs -> call_fresh @@ fun q28 -> 
    (step === pair fromN toN) &&&
    (fromN =/= toN) &&&
    (get fromN state (x % xs)) &&& 
    (get toN state q28) &&&
    (conde [
     (call_fresh @@ fun q29 -> 
        (q28 === nil ()) &&&
        (set fromN xs state q29) &&& 
        (set toN (x % (nil ())) q29 q22));
     (call_fresh @@ fun y -> call_fresh @@ fun ys -> call_fresh @@ fun q33 -> 
        (q28 === y % ys) &&&
        (less x y) &&&
        (set fromN xs state q33) &&& 
        (set toN (x % (y % ys)) q33 q22))
    ])


let rec check state steps = conde [
    (steps === nil ()) &&& (get (one ()) state (nil ())) &&& (get (two ()) state (nil ()));
    call_fresh @@ fun x -> call_fresh @@ fun xs -> call_fresh @@ fun q20 -> 
     (steps === (x % xs)) &&&
     (one_step x state q20) &&&
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
   OCanren.fair_transform @@ check (gen 3) q
  )
