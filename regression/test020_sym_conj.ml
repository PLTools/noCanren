open GT

open MiniKanren
open MiniKanrenStd

open Tester

let rec foo e l =
   (l === nil ()) ||| (fresh (ls) ((l === e % ls) &&& (cont_delay @@ foo e ls)))

let main_goal q = foo !!"A" q <&> foo !!"B" q

let run x = runR (List.reify MiniKanren.reify)
                 (show List.ground @@ show string)
                 (show List.logic (show logic @@ show string)) x


let () = run (-1) q qh ("example", fun q -> main_goal q);;

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

let () =
  run (-1) q qh ("tree", fun q -> main_goal q)
