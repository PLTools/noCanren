open GT
open MiniKanren

@type lam = X of string logic | App of lam logic * lam logic | Abs of string logic * lam logic with show
@type typ = V of string logic | Arr of typ logic * typ logic  with show

open Tester.M

let rec lookupo a g t =
  fresh (a' t' tl)
    (g === !(a', t')%tl)
    (conde [
      (a' === a) &&& (t' === t);
      lookupo a tl t
     ])


let infero expr typ =
  let rec infero gamma expr typ =
    conde [
      fresh (x)
        (expr === !(X x))
        (lookupo x gamma typ);
      fresh (m n t)
        (expr === !(App (m, n)))
        (infero gamma m !(Arr (t, typ)))
        (infero gamma n t);
      fresh (x l t t')
        (expr === !(Abs (x, l)))
        (typ  === !(Arr (t, t')))
        (infero (!(x, t)%gamma) l t')
    ]
  in
  infero !Nil expr typ


let show_env    = GT.( show logic (show llist (show pair (show logic (show string)) (show logic (show typ)))) )
let show_typ    = GT.( show logic (show typ) )
let show_lam    = GT.( show logic (show lam) )
let show_string = GT.( show logic (show string) )

open Tester

let _ =
  run show_typ    empty_reifier 1 q (fun q st -> REPR (lookupo !"x" (of_list []) q                                          st), ["q", q]);
  run show_typ    empty_reifier 1 q (fun q st -> REPR (lookupo !"x" (of_list [!"x", !(V !"x")]) q                           st), ["q", q]);
  run show_typ    empty_reifier 1 q (fun q st -> REPR (lookupo !"x" (of_list [!"y", !(V !"y"); !"x", !(V !"x")]) q          st), ["q", q]);

  run show_string empty_reifier 1 q (fun q st -> REPR (lookupo q (of_list [!"y", !(V !"y"); !"x", !(V !"x")]) !(V !"x")     st), ["q", q]);
  run show_string empty_reifier 1 q (fun q st -> REPR (lookupo q (of_list [!"y", !(V !"y"); !"x", !(V !"x")]) !(V !"y")     st), ["q", q]);
  run show_env    empty_reifier 1 q (fun q st -> REPR (lookupo !"x" q !(V !"y")                                             st), ["q", q]);
  run show_env    empty_reifier 5 q (fun q st -> REPR (lookupo !"x" q !(V !"y")                                             st), ["q", q]);
  run show_typ    empty_reifier 1 q (fun q st -> REPR (infero !(Abs (!"x", !(X !"x"))) q                                    st), ["q", q]);
  run show_typ    empty_reifier 1 q (fun q st -> REPR (infero !(Abs (!"f", !(Abs (!"x", !(App (!(X !"f"), !(X !"x"))))))) q st), ["q", q]);
  run show_typ    empty_reifier 1 q (fun q st -> REPR (infero !(Abs (!"x", !(Abs (!"f", !(App (!(X !"f"), !(X !"x"))))))) q st), ["q", q]);
  run show_lam    empty_reifier 1 q (fun q st -> REPR (infero q !(Arr (!(V !"x"), !(V !"x")))                               st), ["q", q]);
  run show_typ    empty_reifier 1 q (fun q st -> REPR (infero !(Abs (!"x", !(App (!(X !"x"), !(X !"x")))))                q st), ["q", q]);
  ()
