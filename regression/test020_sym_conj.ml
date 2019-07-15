open GT

open MiniKanren
open MiniKanrenStd

open Tester

(* let rec foo e l =
   (l === nil ()) ||| (fresh (ls) ((l === e % ls) &&& (cont_delay @@ foo e ls)))

let main_goal q = foo !!"A" q <&> foo !!"B" q

let run x = runR (List.reify MiniKanren.reify)
                 (show List.ground @@ show string)
                 (show List.logic (show logic @@ show string)) x


(* let () = run (-1) q qh ("example", fun q -> main_goal q);; *)
;;
(*******************************************************************)

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
    fresh (t')
      (t === node t' (leaf ()) <&> cont_delay @@ ltree t')]

let rec rtree t =
  conde [
    t === leaf ();
    fresh (t')
      (t === node (leaf ()) t' <&> cont_delay @@ rtree t')]

let main_goal q = ltree q <&> rtree q


let rec tree_reify x = For_tree.reify tree_reify x
let rec show_tree  x = show tree show_tree x
let rec show_ltree x = show logic (show tree show_ltree) x

let run x = runR tree_reify show_tree show_ltree x

(* let () =
  run (-1) q qh ("tree", fun q -> main_goal q);; *)
;;
(*******************************************************************)

(* let (<&>) = (&&&) *)

let rec appendo x y xy =
   conde [
      (x === nil ()) <&> (y === xy);
      fresh (e x' xy')
        ((x === e % x') <&> (xy === e % xy') <&> (cont_delay @@ appendo x' y xy'))
   ]

let rec reverso x y =
  conde [
    (x === nil ()) <&> (y === nil ());
    fresh (e x' y')
       ((x === e % x') <&> (cont_delay @@ reverso x' y') <&> (cont_delay @@ appendo y' (e % nil ()) y))
       (* ((x === e % x') <&> (cont_delay @@ appendo y' (e % nil ()) y) <&> (cont_delay @@ reverso x' y')) *)
  ]


let rec l = function
  | []      -> nil ()
  | x :: xs -> !!x % l xs


let run x = runR (List.reify MiniKanren.reify)
                 (show List.ground @@ show string)
                 (show List.logic (show logic @@ show string)) x

let list_init n f =
  let rec helper i =
    if i >= n then
      []
    else
      f i :: helper (i+1)
  in
  helper 0

  let () =
    run (-1) q qh ("appendo", fun q -> fresh (p) (appendo q (p % nil ()) (l @@ list_init 10 (Printf.sprintf "%d"))));;


(* let () =
  run (-1) q qh ("rev", fun q -> reverso q (l @@ list_init 10 (Printf.sprintf "%d")));; *)

(*******************************************************************)

(* let () =
  run (-1) q qh ("bad_case", fun q -> (cont_delay success ||| cont_delay failure) <&> (q === nil ()));; *)

(*******************************************************************)


let (&&&) = (<&>)
(* let cont_delay x = x *)

let rec a_star a l = cont_delay @@
  conde [
    l === nil ();
    fresh (ls)
      ((l === a % ls) &&& a_star a ls)
  ]

let rec appendo x y xy = cont_delay @@
   conde [
      (x === nil ()) &&& (y === xy);
      fresh (e x' xy')
        ((x === e % x') &&& (xy === e % xy') &&& (appendo x' y xy'))
   ]

let rec aNbN a b l = cont_delay @@
  conde [
    l === nil ();
    fresh (ls ls')
      ((l === a % ls) &&& appendo ls' (b % nil ()) ls &&& aNbN a b ls')
  ]

let aNbNcM l = cont_delay @@
  fresh (l1 l2)
    (appendo l1 l2 l &&& aNbN !!"A" !!"B" l1 &&& a_star !!"C" l2)

let aMbNcN l = cont_delay @@
  fresh (l1 l2)
    (appendo l1 l2 l &&& a_star !!"A" l1 &&& aNbN !!"B" !!"C" l2)

let aNbNcN l = aNbNcM l &&& aMbNcN l *)

(* let () =
  run (5) q qh ("aNbNcN", fun q -> fresh (p) (aNbNcN q)) *)


(*******************************************************************)

let run x = runR (List.reify MiniKanren.reify)
                 (show List.ground @@ show string)
                 (show List.logic (show logic @@ show string)) x

let rec inc name var =
  var := 1 + !var;
  if !var mod 1000 = 0 then
    Printf.printf "%s = %d\n%!" name !var;
  success <&> (delay @@ fun () -> cont_delay @@ inc name var)

let () =
  let lst = List.init 10 (fun i -> (Printf.sprintf "n%d" i, ref 0)) in
  let goal = List.fold_right (fun (n, v) g -> inc n v <&> g) lst success in
  run (1) q qh ("hh", fun q -> goal)
