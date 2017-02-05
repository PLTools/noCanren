(* This is a test suite for a lot of disequality constaints
 * mixed with plain OCaml lists
 *)
open Printf
open MiniKanren
open Tester

let (!) x = inj@@lift x

(* reifier for ints *)
let show_int = string_of_int
let show_if : (int,int)fancy -> string = fun x -> show_fancy string_of_int x

let show_il x =
  show_logic string_of_int x

let runInt = runR intl_of_intf show_int show_il;;

@type 'a gt = N | A of 'a with show;;
type rt = rt gt             (* autoreified *)
type ft = (ft gt, rt) fancy (* fancified *)
type lt = lt gt logic       (* reified *)

let show_rt x =
  let rec helper : rt -> string = function
  | N -> "N"
  | A x -> "A (" ^ (helper x) ^ ")"
  in
  helper x

let show_ln (x: lt) : string =
  let rec helper: lt  -> string = function
  | Value N -> "N"
  | Value (A x) -> "A (" ^ (show_logic helper2 x) ^ ")"
  | Var _ as v -> show_logic helper2 v
  and helper2: lt gt -> string = function
  | N -> "N"
  | A x -> "A (" ^ (helper x) ^ ")"
  in
  helper x

let lt_of_ft (c: var_checker) ft : lt =
  let rec helper x =
    if c#isVar x
    then (refine_fancy x c helper)
    else match coerce_fancy x with
    | N -> Value N
    | A x -> Value (A (helper x))
  in
  helper ft

module N = struct
  type 'a t = 'a gt
  type r = rt
end
module NF = FMapALike2(N)

let a x : ft = NF.wrap @@ inj@@lift (A x)
let n :   ft = NF.wrap @@ inj@@lift N

let _ =
  runInt          (-1) q (REPR (fun q -> (fresh(x) (x =/= (a x)))         )) qh;
  ()

let _ =
  runInt       (-1) q (REPR (fun q -> (q =/= !1) )         ) qh;
  runInt       (-1) q (REPR (fun q -> (fresh (x y z)(x =/= y)(x === ![!0; z; !1])(y === ![!0; !1; !1]))                   )) qh;
  runInt       (-1) q (REPR (fun q -> (fresh (x y z)(x =/= y)(x === ![!0; z; !1])(y === ![!0; !1; !1])(z === !0))         )) qh;
  runInt       (-1) q (REPR (fun q -> (fresh (x y z)(z === !0)(x =/= y)(x === ![!0; z; !1])(y === ![!0; !1; !1]))         )) qh;
  runInt       (-1) q (REPR (fun q -> (fresh (x y z)(z === !1)(x =/= y)(x === ![!0; z; !1])(y === ![!0; !1; !1]))         )) qh;
  ()

let _ =
  runInt       (-1) q (REPR (fun q -> (fresh (x y z)(x === ![!0; z; !1])(y === ![!0; !1; !1])(x =/= y))                   )) qh;
  runInt       (-1) q (REPR (fun q -> (fresh (x y z)(z === !1)(x === ![!0; z; !1])(y === ![!0; !1; !1])(x =/= y))         )) qh;
  ()
let _ =
  runInt      (-1) q (REPR (fun q -> (fresh (x y)(![x; !1] =/= ![!2; y])(x === !2))                                       )) qh;
  runInt      (-1) q (REPR (fun q -> (fresh (x y)(![x; !1] =/= ![!2; y])(y === !1))                                       )) qh;
  ()
;;

let show1 l  = (GT.show(List.logic) (GT.show(logic) string_of_int)) l
let show_ilist = GT.show(GT.list) @@ GT.show(GT.int)

let show_iflist: (int, int) fancy list -> _ = GT.show(GT.list) @@ (Obj.magic @@ (GT.show(GT.int)))

let ilist_of_ftyp2 (c: var_checker) y =
  let rec helper (t: ( (int,int) fancy list as 'l,'l) fancy) : int logic MiniKanren.List.logic =
    if c#isVar t
    then refine_fancy t c helper
    else helper2 @@ coerce_fancy t
  and helper2 (t: (int,int) fancy list) =
    match t with
    | [] -> Value Nil
    | h :: tl ->
      let h2 =
        if c#isVar h then refine_fancy h c (intl_of_intf c)
        else Value (coerce_fancy h)
      in
      Value (Cons (h2, helper2 tl))
  in
  helper y

(* let (_:int) = ilist_of_ftyp2 *)
let runIntList n = runR ilist_of_ftyp2 show_iflist show1 n

let _ =
  (* run_exn show_int       (-1) q (REPR (fun _ -> (fresh (x y)(![x; !1] =/= ![!2; y])(x === !2)(y === !1))                                  )) qh; *)
  runIntList      (-1) q (REPR (fun q -> (fresh (x y)
                                                    (![x; !1] =/= ![!2; y])
                                                    (![x; y] === q))
                                                  )) qh;



  runIntList      (-1) q (REPR (fun q -> (fresh (x y)(![x; !1] =/= ![!2; y])(x === !2)(![x; y] === q))                             )) qh;
  run_exn show_iflist      (-1) q (REPR (fun q -> (fresh (x y)(![x; !1] =/= ![!2; y])(x === !2)(y === !9)(![x; y] === q))                   )) qh;
  run_exn show_iflist      (-1) q (REPR (fun q -> (fresh (a d)(![a; d] === q)(q =/= ![!5; !6])(a === !5)(d === !6))                         )) qh;
  runIntList      (-1) q (REPR (fun q -> (fresh (x y)(![x; !1] =/= ![!2; y])(x === !2)(y === !1)(![x; y] === q))                   )) qh;
  run_exn show_iflist      (-1) q (REPR (fun q -> (fresh (a x z)(a =/= ![x; !1])(a === ![z; !1])(x === z))                                  )) qh;
  ()

let _ =
  runIntList      (-1) q (REPR (fun q -> (fresh (a x z)(a =/= ![x; !1])(a === ![z; !1])(x === !5)(![x; z] === q))                  )) qh;
  ()
let _ =
  runInt                (-1) q (REPR (fun q -> (!3 =/= !4) )) qh;
  runInt                (-1) q (REPR (fun q -> (!3 =/= !3)                                              )) qh;
  run_exn show_int      (-1) q (REPR (fun q -> ((!5 =/= q) &&& (!6 =/= q) &&& (q === !5))               )) qh;
  runIntList            (-1) q (REPR (fun q -> (fresh (a d)(![a; d] === q)(q =/= ![!5; !6])(a === !5))  )) qh;
  runInt                (-1) q (REPR (fun q -> (fresh (a)(!3 === a)(a =/= !4))                          )) qh;
  runInt                (-1) q (REPR (fun q -> ((!4 =/= q) &&& (!3 =/= q))                              )) qh;
  runInt                (-1) q (REPR (fun q -> ((q =/= !5) &&& (q =/= !5))                              )) qh;

  runInt                (-1) q (REPR (fun q -> (let foo x = fresh (a)(x =/= a) in fresh(a)(foo a))      )) qh;
  runIntList            (-1) q (REPR (fun q -> (fresh (x y)(![x; y] === q)(x =/= y))                                                     )) qh;
  runIntList            (-1) q (REPR (fun q -> Fresh.two (fun a d -> ?& [ ![a; d] === q; q =/= ![!5; !6] ]) )) qh;
  runIntList            (-1) q (REPR (fun q -> Fresh.two (fun a d -> ?& [ ![a; d] === q; q =/= ![!5; !6];
                                                                          (a === !3) ])                    )) qh;
  runIntList            (-1) q (REPR (fun q -> (fresh (x y)(![x; y] === q)(y =/= x))                    )) qh;
  runIntList            (-1) q (REPR (fun q -> (fresh (x y)(![x; y] === q)(x =/= y)(y =/= x))           )) qh;
  runIntList            (-1) q (REPR (fun q -> (fresh (x y)(![x; y] === q)(x =/= y)(x =/= y))           )) qh;
  runInt                (-1) q (REPR (fun q -> ((q =/= !5) &&& (!5 =/= q))                  )) qh;

  runIntList           (-1) q (REPR (fun q -> (fresh (x y)(![x; y] === q)(![x; y] =/= ![!5; !6])(x =/= !5))                             )) qh;
  runIntList           (-1) q (REPR (fun q -> (fresh (x y)(![x; y] === q)(x =/= !5)(![x; y] =/= ![!5; !6]))                             )) qh;
  runIntList           (-1) q (REPR (fun q -> (fresh (x y)(x =/= !5)(![x; y] =/= ![!5; !6])(![x; y] === q))                             )) qh;
  runIntList           (-1) q (REPR (fun q -> (fresh (x y)(!5 =/= x)(![x; y] =/= ![!5; !6])(![x; y] === q))                             )) qh;
  runIntList           (-1) q (REPR (fun q -> (fresh (x y)(!5 =/= x)(![y; x] =/= ![!6; !5])(![x; y] === q))                             )) qh;
  runIntList           (-1) q (REPR (fun x -> (fresh (y z)(x =/= ![y; !2])(x === ![z; !2]))                                             )) (fun xs -> ["x", xs]);
  ()


let show_int_list = GT.show(GT.list) @@ GT.show(GT.int)
let () =
  let rec distincto l =
    conde [
      l === nil ();
      (fresh (a) (l === !< a));
      (fresh (a ad dd) (
         (l === a % (ad % dd)) &&&
         (a =/= ad) &&&
         (distincto (a % dd)) &&&
         (distincto (ad % dd))
      ))
    ]
  in
  runInt (-1) q (REPR (fun q -> distincto (!2 % (!3 %< q)))) qh;

   let rec remembero x ls out =
     conde [
       (ls === nil ()) &&& (out === nil ());
       fresh (a d res) (
         (ls === a % d) &&&
         (remembero x d res) &&&
         (conde [
             (a === x) &&& (out === res);
             (out === a % res)
          ])
       )
     ]
   in
   run_exn show_int_list (-1) q (REPR (fun q -> remembero !1 (!1 % (!2 % (!1 %< !3))) q         )) qh;
   runInt  (-1) q (REPR (fun q -> remembero !1 (!1 % (!2 %< !3)) (!1 % (!2 %< !3)))) qh;

   let rec rembero x ls out =
     conde [
       (ls === nil ()) &&& (out === nil ());
       fresh (a d res) (
         (ls === a % d) &&&
         (rembero x d res) &&&
         (conde [
             (a === x) &&& (out === res);
             (a =/= x) &&& (out === a % res)
          ])
       )
     ]
   in
   run_exn show_int_list (-1) q (REPR (fun q -> rembero !1 (!1 % (!2 % (!1 %< !3))) q         )) qh;
   runInt                (-1) q (REPR (fun q -> rembero !1 (!1 % (!2 %< !3)) (!1 % (!2 %< !3)))) qh;
   ()
