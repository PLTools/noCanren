open MiniKanren
open MiniKanrenStd
open Unify
open Tester
open GT

let show_number x =
  let rec nat2int =
    function
    | O   -> 0
    | S x -> 1 + nat2int x in
  Printf.sprintf "%d" @@ nat2int x

let show_lnumber x =
  let rec nat2int x =
    match x with
    | Var _       -> 0, Some (show(logic) (fun _ -> "") x)
    | Value O     -> 0, None
    | Value (S n) -> let a, s = nat2int n in a + 1, s in
  match nat2int x with
  | (n, None)   -> Printf.sprintf "%d" n
  | (0, Some s) -> Printf.sprintf "%s" s
  | (n, Some s) -> Printf.sprintf "(%d + %s)" n s

let show_llist f x =
  let rec show_list x = Printf.sprintf "[%s]" (String.concat "; " x) in
  let rec show_llist x =
    match x with
    | Var _               -> [], Some (show(logic) (fun _ -> "") x)
    | Value Nil           -> [], None
    | Value (Cons (x,xs)) -> let l, q = show_llist xs in f x :: l, q in
  match show_llist x with
  | [], None   -> "[]"
  | [], Some s -> s
  | x , None   -> show_list x
  | x , Some s -> Printf.sprintf "%s ^ %s" (show_list x) s


let rec show_gterm f g = function
| Var_ v        -> Printf.sprintf "V %s" (f v)
| Constr (n, a) -> Printf.sprintf "C %s %s" (f n) (g a)

let rec show_term f x = show_gterm f (show(List.ground) (show_term f)) x
let rec show_lterm f x = show(logic) (show_gterm f (show (List.logic) (show_lterm f))) x

let rec int2nat n = if n <= 0 then O else S (int2nat (n - 1))
let v n   = Var_ (int2nat n)
let c n a = Constr (int2nat n, a)

let t1 = c 0 []
let t2 = v 0
let t2' = c 0 [t2]

let t3 = c 0 [v 0; c 1 []]
let t4 = c 0 [c 1 []; v 0]

let t5 = c 0 [c 1 []; v 1]

let x1 = c 0 [v 0              ; c 1 [v 2; c 3 []]]
let x2 = c 0 [c 1 [c 2 []; v 3]; v 1              ]

let x1' = c 0 [v 0; v 0              ; c 1 [v 2; c 3 []]]
let x2' = c 0 [v 1; c 1 [c 2 []; v 3]; v 1              ]

let y1 = c 0 [v 0; v 1; v 2; v 0                        ; v 1                        ; v 2                        ; v 3                        ]
let y2 = c 0 [v 1; v 2; v 3; c 1 [c 2 []; v 5; v 6; v 7]; c 1 [v 4; c 3 []; v 6; v 7]; c 1 [v 4; v 5; c 4 []; v 7]; c 1 [v 4; v 5; v 6; c 5 []]]

let w1, w2 =
  let appendo x y z = c 1 [x; y; z] in
  let cons x y = c 2 [x; y] in
  let nil = c 3 [] in
  let list2 x y = cons x (cons y nil) in
  let cA = c 4 [] in
  let cB = c 5 [] in
  let cC = c 6 [] in
  let cD = c 7 [] in
  let vX = v 0 in
  let vXs = v 1 in
  let vYs = v 2 in
  let vZs = v 3 in
  let vLs = v 4 in
  appendo (list2 cA cB) (list2 cC cD) (vLs),        (* append([a, b]  , [c, d], Ls      ) *)
  appendo (cons vX vXs) (vYs)         (cons vX vZs) (* append([X | Xs], Ys    , [X | Zs]) *)

let rec (!!!) =
  let rec n2l = function
  | O   -> o ()
  | S x -> s (n2l x) in
  let rec l2l e2l = function
  | []    -> nil ()
  | x::xs -> e2l x % l2l e2l xs in
function
| Var_ v         -> var_ (n2l v)
| Constr (n, a) -> constr (n2l n) (l2l (!!!) a)

let check_uni t1 t2 s = check_uni s !!!t1 !!!t2

let rec init_subst subst n =
  if n = 0
    then subst === nil ()
    else fresh (x xs)
           (subst === x % xs)
           (init_subst xs (n-1))

let my_show x = show(List.ground) (show_term show_number) x
let my_lshow x = show_llist (show_lterm show_lnumber) x
let rec nat_reifier x = For_gnat.reify nat_reifier x
let rec term_reifier r x = For_gterm.reify r (List.reify (term_reifier r)) x
let my_reifier = List.reify ( (term_reifier nat_reifier))

let rec l2ll = function
| []    -> nil ()
| x::xs -> x % l2ll xs

let res1 = l2ll [some (!!! (v 1)); some (!!! (c 1 [v 2; v 3])); some (!!!(c 2 [])); some (!!!(c 3 []))]

let full_run = runR my_reifier my_show my_lshow

let () =
  (* full_run (-1) q qh ("answers1", (fun q ->fresh (x) (q === none ()% x) ));
  full_run (-1) q qh ("answers1", (fun q ->fresh (x) (q === none ()% (x % nil ()))));
  full_run (-1) q qh ("answers1", (fun q -> fresh (x) (q === some (var_ (s (s x))) % nil ()))); *)
  full_run (-1) q qh ("answers1", (fun q -> check_uni t1 t1 q !!true));
  full_run (-1) q qh ("answers2", (fun q -> check_uni t2 t2 q !!true));
  full_run (-1) q qh ("answers3", (fun q -> check_uni t1 t2 q !!true));
  full_run (-1) q qh ("answers4", (fun q -> check_uni t3 t4 q !!true));
  full_run (-1) q qh ("answers5", (fun q -> check_uni t3 t5 q !!true));
  full_run (-1) q qh ("answers6", (fun q -> check_uni t4 t5 q !!true));
  full_run (-1) q qh ("answers7", (fun q -> check_uni x1 x2 q !!true));
  full_run (-1) q qh ("answers8", (fun q -> check_uni x1' x2' q !!true));
  full_run (-1) q qh ("answers9", (fun q -> check_uni y1 y2 q !!true));


  full_run (-1) q qh ("answers_bad", (fun q -> check_uni t2 t2' q !!true));

  full_run (-1) q qh ("answers", (fun q -> check_uni w1 w2 q !!true));
  ()
