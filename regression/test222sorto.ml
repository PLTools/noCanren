open GT
open MiniKanren
open MiniKanren.Deep
open Tester
open Tester.Deep

let (!) = (!!)
(*** Sorting ***)

let id_set2 = IdentSet.empty ()

let x   : Nat.logic            = IdentSet.new_ident id_set2 0
let y   : Nat.logic            = IdentSet.new_ident id_set2 1
let x'  : Nat.logic            = IdentSet.new_ident id_set2 2
let y'  : Nat.logic            = IdentSet.new_ident id_set2 3
let min : Nat.logic            = IdentSet.new_ident id_set2 4
let max : Nat.logic            = IdentSet.new_ident id_set2 5
let l   : Nat.logic List.logic = IdentSet.new_ident id_set2 6
let s   : Nat.logic            = IdentSet.new_ident id_set2 7
let l'  : Nat.logic List.logic = IdentSet.new_ident id_set2 8
let h   : Nat.logic            = IdentSet.new_ident id_set2 9
let t   : Nat.logic List.logic = IdentSet.new_ident id_set2 10
let s'  : Nat.logic            = IdentSet.new_ident id_set2 11
let t'  : Nat.logic List.logic = IdentSet.new_ident id_set2 12
let xs  : Nat.logic List.logic = IdentSet.new_ident id_set2 13
let ys  : Nat.logic List.logic = IdentSet.new_ident id_set2 14
let ss  : Nat.logic List.logic = IdentSet.new_ident id_set2 15
let xs' : Nat.logic List.logic = IdentSet.new_ident id_set2 16
let ys' : Nat.logic List.logic = IdentSet.new_ident id_set2 17
let m   : Nat.logic            = IdentSet.new_ident id_set2 18
let zs  : Nat.logic List.logic = IdentSet.new_ident id_set2 19
let z   : Nat.logic            = IdentSet.new_ident id_set2 20
let z'  : Nat.logic            = IdentSet.new_ident id_set2 21

let  lto = def "lto" (x ^. y) (
  conde [
    fresh (!^ y') [(x === !O) &&& (y === !(S y'))];
    fresh (x' ^. y') [
      (x === !(S x'));
      (y === !(S y'));
      (invoke "lto" (x' ^. y'))
    ]
  ]
)

let geo = def "geo" (x ^. y) (
  (x === y) ||| (invoke "lto" (y ^. x))
)

let minimumo = def "minimumo" (xs ^. m) (
  conde [
    (xs === !< m);
    fresh (x ^~ t ^. y) [
      (xs === x % t);
      (invoke "minimumo" (t ^. y));
      (conde [
        (invoke "lto" (x ^. y)) &&& (x === m);
        (invoke "geo" (x ^. y)) &&& (y === m)
      ])
    ]
  ]
)

let minmaxo = def "minmaxo" (x ^~ y ^~ min ^. max) (
  conde [
    (x === min) &&& (y === max) &&& (invoke "lto" (x ^. y));
    (y === min) &&& (x === max) &&& (invoke "geo" (x ^. y))
  ]
)

let smallesto = def "smallesto" (l ^~ s ^. l') (
  conde [
    (l === !< s) &&& (l' === !Nil);
    fresh (h ^~ t ^~ s' ^~ t' ^. max) [
      (l === h % t);
      (invoke "smallesto" (t ^~ s' ^. t'));
      (invoke "minmaxo" (h ^~ s' ^~ s ^. max));
      (l' === max % t')
    ]
  ]
)

let sorto = def "sorto" (xs ^. ys) (
  conde [
    (xs === !Nil) &&& (ys === !Nil);
    fresh (s ^~ xs' ^. ys') [
      (invoke "smallesto" (xs ^~ s ^. xs'));
      (invoke "sorto" (xs' ^. ys'));
      (ys === s % ys')
    ]
  ]
)

let permo = def "permo" (xs ^. ys) (
  fresh (!^ ss) [
    (invoke "sorto" (xs ^. ss)) &&& (invoke "sorto" (ys ^. ss))
  ]
)



(*** Peano arithmetic ***)

let pluso = def "pluso" (x ^~ y ^. z) (
  conde [
    (x === !O) &&& (y === z);
    fresh (x' ^. z') [
      (x === !(S x'));
      (z === !(S z'));
      (invoke "pluso" (x' ^~ y ^. z'))
    ]
  ]
)

let mulo = def "mulo" (x ^~ y ^. z) (
  conde [
    (x === !O) &&& (z === !O);
    fresh (!^ x') [(x === !(S x')) &&& (y === !O) &&& (z === !O)];
    fresh (x' ^~ z' ^. y') [
      (x === !(S x'));
      (y === !(S y'));
      (invoke "mulo"  (x' ^~ y ^. z'));
      (invoke "pluso" (z' ^~ y ^. z))
    ]
  ]
)

let map_succ = def "map_succ" (xs ^. ys) (
  conde [
    (xs === !Nil) &&& (ys === !Nil);
    fresh (x ^~ xs' ^~ y ^. ys') [
      (xs === x % xs');
      (y === !(S x));
      (invoke "map_succ" (xs' ^. ys'));
      (ys === y % ys')
    ]
  ]
)

let show_nat_gr      n = show int @@ prj_nat n
let show_nat_list_gr l = show list (show int) @@ prj_nat_list l

let minimumo_list  = [geo; lto; minimumo]
let sorto_list     = [lto; geo; minmaxo; smallesto; sorto]
let permo_list = [lto; geo; minmaxo; smallesto; sorto; permo]

let _ =
  run show_nat_gr      (-1) q   ("minimumo", (fun q     -> prog id_set2 minimumo_list  (invoke "minimumo"  (inj_nat_list [6; 5; 4; 1; 2] ^. q))                  )) qh;
  run show_nat_gr      (-1) q   ("minimumo", (fun q     -> prog id_set2 minimumo_list  (invoke "minimumo"  (inj_nat_list [] ^. q))                               )) qh;
(*
  run show_nat_list_gr (-1) q   (REPR (fun q     -> prog id_set2 sorto_list     (invoke "sorto"     (inj_nat_list [8; 9; 10; 7; 6; 5; 4; 1; 2; 3] ^. q))  )) qh;
*)

  run show_nat_list_gr (-1) q   ("sorto", (fun q     -> prog id_set2 sorto_list     (invoke "sorto"     (q ^. inj_nat_list [1; 2; 3; 4; 5]))               )) qh;
  run show_nat_list_gr (-1) q   ("permo", (fun q     -> prog id_set2 permo_list     (invoke "permo"     (inj_nat_list [4; 3; 5; 5; 1] ^. q))               )) qh;
