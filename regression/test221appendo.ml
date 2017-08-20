open GT
open MiniKanren
open MiniKanren.Deep
open Tester
open Tester.Deep

let (!) = (!!)
(*** Lists ***)

let id_set1 = IdentSet.empty ()

let a   : int logic List.logic = IdentSet.new_ident id_set1 0
let b   : int logic List.logic = IdentSet.new_ident id_set1 1
let ab  : int logic List.logic = IdentSet.new_ident id_set1 2
let h   : int logic            = IdentSet.new_ident id_set1 3
let t   : int logic List.logic = IdentSet.new_ident id_set1 4
let a'  : int logic List.logic = IdentSet.new_ident id_set1 5
let ab' : int logic List.logic = IdentSet.new_ident id_set1 6
let hs  : int logic List.logic = IdentSet.new_ident id_set1 7


let appendo = def "appendo" (a ^~ b ^. ab) (
  conde [
    (a === !Nil) &&& (b === ab);
    fresh (h ^. t) [
      (a === h % t);
      fresh (!^ ab') [(h % ab' === ab) &&& (invoke "appendo" (t ^~ b ^. ab'))]
    ]
  ]
)

let reverso = def "reverso" (a ^. b) (
  disj
    (conj (a === !Nil) (b === !Nil))
    (fresh (h ^. t) [
        conj (a === h % t)
             (fresh (hs ^. a') [
                   (hs === (!< h));
                   (invoke "reverso" (t ^. a'));
                   (invoke "appendo" (a' ^~ hs ^. b))
              ])
    ])
)


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

let lto = def "lto" (x ^. y) (
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

let show_int           = show (logic) (show int)
let show_int_list      = show (List.logic) show_int
let show_nat           = show (Nat.logic)
let show_nat_list      = show (List.logic) show_nat
let show_nat_gr      n = show int @@ prj_nat n
let show_nat_list_gr l = show list (show int) @@ prj_nat_list l

let reverso_list   = [appendo; reverso]

let rec nat_pref = function
| 0 -> []
| n -> n :: nat_pref (n - 1)

let _ =
  run show_int_list    (-1) q   (REPR (fun q     -> prog id_set1 [appendo ]     (invoke "appendo"   (inj_list [1; 2] ^~ inj_list [3; 4]  ^. q))           )) qh;
  run show_int_list    (-1) q   (REPR (fun q     -> prog id_set1 [appendo ]     (invoke "appendo"   (q ^~ inj_list [3; 4]  ^. inj_list [1; 2; 3; 4]))     )) qh;
  run show_int_list    (-1) q   (REPR (fun q     -> prog id_set1 [appendo ]     (invoke "appendo"   (inj_list [1; 2] ^~ q  ^. inj_list [1; 2; 3; 4]))     )) qh;
  run show_int_list    (-1) qr  (REPR (fun q r   -> prog id_set1 [appendo ]     (invoke "appendo"   (q ^~ r  ^. inj_list [1; 2; 3; 4]))                   )) qrh;
  run show_int_list    (-1) q   (REPR (fun q     -> prog id_set1 reverso_list   (invoke "reverso"   (q ^. inj_list (nat_pref 70)))                      )) qh;
  run show_int_list    (-1) q   (REPR (fun q     -> prog id_set1 reverso_list   (invoke "reverso"   (inj_list [1; 2; 3; 4; 5] ^. q))                      )) qh;
  run show_int_list    (-1) q   (REPR (fun q     -> prog id_set1 reverso_list   (invoke "reverso"   (inj_list [1; 2; 3] ^. inj_list [3; 2; 1]))           )) qh;
  run show_int_list    (-1) q   (REPR (fun q     -> prog id_set1 reverso_list   (invoke "reverso"   (inj_list [1; 2; 1] ^. inj_list [3; 2; 1]))           )) qh;
