open GT
open MiniKanren
open MiniKanren.Deep
open Tester
open Tester.Deep

let (!) = (!!)



(*** Binary arithmetic ***)

let id_set3 = IdentSet.empty ()

let x   : int logic List.logic = IdentSet.new_ident id_set3 0
let y   : int logic List.logic = IdentSet.new_ident id_set3 1
let r   : int logic List.logic = IdentSet.new_ident id_set3 2
let x'  : int logic List.logic = IdentSet.new_ident id_set3 3
let y'  : int logic List.logic = IdentSet.new_ident id_set3 4
let r'  : int logic List.logic = IdentSet.new_ident id_set3 5
let h   : int logic            = IdentSet.new_ident id_set3 6
let t   : int logic List.logic = IdentSet.new_ident id_set3 7
let r'' : int logic List.logic = IdentSet.new_ident id_set3 8
let d   : int logic List.logic = IdentSet.new_ident id_set3 9
let q'  : int logic List.logic = IdentSet.new_ident id_set3 9
let n   : int logic List.logic = IdentSet.new_ident id_set3 10
let n'  : int logic List.logic = IdentSet.new_ident id_set3 11
let h'  : int logic            = IdentSet.new_ident id_set3 12

(*let m   : Nat.logic            = IdentSet.new_ident id_set2 18*)
(*let zs  : Nat.logic List.logic = IdentSet.new_ident id_set2 19*)
let z   : Nat.logic            = IdentSet.new_ident id_set3 20
(*let z' : Nat.logic            = IdentSet.new_ident id_set2 21*)

let rec to_bin = (function
| 0 -> !Nil
| n when n mod 2 = 0 -> !0 % to_bin (n / 2)
| n                  -> !1 % to_bin (n / 2)
)

let poso = def "poso" (!^ x) (
  fresh (h ^. t) [
    (x === h % t)
  ]
)

let bin_pluso = def "bin_pluso" (x ^~ y ^. r) (
  conde [
    (y === !Nil) &&& (x === r);
    (x === !Nil) &&& (invoke "poso" (!^ y)) &&& (y === r);
    (fresh (x' ^~ y' ^. r') [
      (x === !0 % x');
      (y === !0 % y');
      (r === !0 % r');
      (invoke "poso" (!^ x'));
      (invoke "poso" (!^ y'));
      (invoke "bin_pluso" (x' ^~ y' ^. r'))
    ]);
    (fresh (x' ^~ y' ^. r') [
      (x === !0 % x');
      (y === !1 % y');
      (r === !1 % r');
      (invoke "poso" (!^ x'));
      (invoke "bin_pluso" (x' ^~ y' ^. r'))
    ]);
    (fresh (x' ^~ y' ^. r') [
      (x === !1 % x');
      (y === !0 % y');
      (r === !1 % r');
      (invoke "poso" (!^ y'));
      (invoke "bin_pluso" (x' ^~ y' ^. r'))
    ]);
    (fresh (x' ^~ y' ^~ r' ^. r'') [
      (x === !1 % x');
      (y === !1 % y');
      (r === !0 % r');
      (invoke "bin_pluso" (x' ^~ y' ^. r''));
      (invoke "bin_pluso" (r'' ^~ (!< (!1)) ^. r'))
    ])
  ]
)

let bin_multo = def "bin_multo" (x ^~ y ^. r) (
  conde [
    (x === !Nil) &&& (r === !Nil);
    (invoke "poso" (!^ x)) &&& (y === !Nil) &&& (r === !Nil);
    (invoke "poso" (!^ x)) &&& (y === !< !1) &&& (r === x);
    (fresh (y' ^. r') [
      (invoke "poso" (!^ x));
      (y === !0 % y');
      (invoke "poso" (!^ y'));
      (invoke "bin_multo" (x ^~ y' ^. r'));
      (r === !0 % r')
    ]);
    (fresh (y' ^~ r' ^. r'') [
      (invoke "poso" (!^ x));
      (y === !1 % y');
      (invoke "poso" (!^ y'));
      (invoke "bin_multo" (x ^~ y' ^. r'));
      (r'' === !0 % r');
      (invoke "bin_pluso" (r'' ^~ x ^. r))
    ])
  ]
)

let bin_lto = def "bin_lto" (x ^. y) (
  fresh (!^ d) [
    (invoke "poso" (!^ d));
    (invoke "bin_pluso" (x ^~ d ^. y))
  ]
)

let bin_divo = def "bin_divo" (x ^~ y ^~ q' ^. r) (
  fresh (!^ z) [
    (invoke "poso" (!^ y));
    (invoke "poso" (!^ z));
    (invoke "bin_lto"   (r ^. y));
    (invoke "bin_multo" (q' ^~ y ^. z));
    (invoke "bin_pluso" (z ^~ r ^. x))
  ]
)

let bin_gt1o = def "bin_gt1o" (!^ x) (
  fresh (h ^~ h' ^. t) [
    (x === h % (h' % t))
  ]
)

let bin_powo = def "bin_powo" (x ^~ y ^. r) (
  conde [
    (x === !Nil) &&& (invoke "poso" (!^ y)) &&& (r === !Nil);
    (x === !< !1) &&& (invoke "poso" (!^ y)) &&& (r === !< !1);
    (invoke "bin_gt1o" (!^ x)) &&& (y === !Nil) &&& (r === !< !1);
    (invoke "bin_gt1o" (!^ x)) &&& (y === !< !1) &&& (r === x);
    (*(invoke "poso" (!^ x)) &&& (y === !< !1) &&& (r === x); *)
    (fresh (y' ^. r') [
      (invoke "bin_gt1o" (!^ x));
      (invoke "bin_gt1o" (!^ r));
      (y === !0 % y');
      (invoke "poso" (!^ y'));
      (invoke "bin_multo" (r' ^~ r' ^. r));
      (invoke "bin_powo" (x ^~ y' ^. r'))
    ]);
    (* (fresh (y' ^~ r' ^. r'') [
      (invoke "bin_gt1o" (!^ x));
      (invoke "bin_lto" ((!< !1) ^. r));
      (y === !1 % y');
      (invoke "poso" (!^ x'));
      (invoke "bin_multo" (r'' ^~ x ^. r));
      (invoke "bin_multo" (r' ^~ r' ^. r''));
      (invoke "bin_powo" (x ^~ y' ^. r'));
    ]) *)
  ]
)

(*let bin_powo = def "bin_powo" (x ^~ n ^. r) (
  conde [
    (n === !Nil) &&& (r === !< !1);
    fresh (n' ^. r') [
      (invoke "bin_pluso" (n' ^~ (!< !1) ^. n));
      (invoke "bin_powo"  (x ^~ n' ^. r'));
      (invoke "bin_multo" (r' ^~ x ^. r))
    ]
  ]
)*)

let show_int = show (logic) (show int)
let show_int_list = show (List.logic) show_int

let bin_pluso_list = [poso; bin_pluso]
let bin_multo_list = [poso; bin_pluso; bin_multo]
let bin_lto_list   = [poso; bin_pluso; bin_lto]
let bin_divo_list  = [poso; bin_pluso; bin_lto; bin_multo; bin_divo]
let bin_powo_list = [poso; bin_pluso; bin_gt1o; bin_multo; bin_powo]

let () =
  run show_int_list    (-1) q   (REPR (fun q     -> prog id_set3 bin_pluso_list (invoke "bin_pluso" ((to_bin 3) ^~ (to_bin 6) ^. q))                      )) qh;
  run show_int_list    (-1) q   (REPR (fun q     -> prog id_set3 bin_pluso_list (invoke "bin_pluso" ((to_bin 2) ^~ q ^. (to_bin 5)))                      )) qh;
  run show_int_list    (-1) q   (REPR (fun q     -> prog id_set3 bin_pluso_list (invoke "bin_pluso" ((to_bin 8) ^~ q ^. (to_bin 6)))                      )) qh;
  run show_int_list    (-1) q   (REPR (fun q     -> prog id_set3 bin_pluso_list (invoke "bin_pluso" (q ^~ (to_bin 5) ^. (to_bin 8)))                      )) qh;
  run show_int_list    (-1) qr  (REPR (fun q r   -> prog id_set3 bin_pluso_list (invoke "bin_pluso" (q ^~ r  ^. (to_bin 5)))                              )) qrh;

  run show_int_list    (-1) q   (REPR (fun q     -> prog id_set3 bin_multo_list (invoke "bin_multo" ((to_bin 5) ^~ (to_bin 3) ^. q))                      )) qh;
  run show_int_list    (-1) q   (REPR (fun q     -> prog id_set3 bin_multo_list (invoke "bin_multo" (q ^~ (to_bin 3) ^. (to_bin 12)))                     )) qh;
  run show_int_list    (-1) q   (REPR (fun q     -> prog id_set3 bin_multo_list (invoke "bin_multo" ((to_bin 3) ^~ q ^. (to_bin 12)))                     )) qh;
  run show_int_list    (-1) qr  (REPR (fun q r   -> prog id_set3 bin_multo_list (invoke "bin_multo" (q ^~ r ^. (to_bin 24)))                              )) qrh;
  run show_int_list    (-1) q   (REPR (fun q     -> prog id_set3 bin_lto_list   (invoke "bin_lto"   ((to_bin 5)  ^. (to_bin 10)))                         )) qh;
  run show_int_list    (-1) q   (REPR (fun q     -> prog id_set3 bin_lto_list   (invoke "bin_lto"   ((to_bin 6)  ^. (to_bin 6) ))                         )) qh;
  run show_int_list    (-1) q   (REPR (fun q     -> prog id_set3 bin_lto_list   (invoke "bin_lto"   ((to_bin 15) ^. (to_bin 4) ))                         )) qh;
  run show_int_list    (-1) q   (REPR (fun q     -> prog id_set3 bin_lto_list   (invoke "bin_lto"   (q           ^. (to_bin 13)))                         )) qh;
  run show_int_list    (10) q   (REPR (fun q     -> prog id_set3 bin_lto_list   (invoke "bin_lto"   ((to_bin 13) ^. q))                                   )) qh;
  run show_int_list    (-1) qr  (REPR (fun q r   -> prog id_set3 bin_divo_list  (invoke "bin_divo"  ((to_bin 21) ^~ (to_bin 7) ^~ q ^. r))                )) qrh;
  run show_int_list    (-1) qr  (REPR (fun q r   -> prog id_set3 bin_divo_list  (invoke "bin_divo"  ((to_bin 23) ^~ (to_bin 5) ^~ q ^. r))                )) qrh;
  run show_int_list    (-1) qrs (REPR (fun q r s -> prog id_set3 bin_divo_list  (invoke "bin_divo"  ((to_bin 16) ^~ q ^~ r ^. s))                         )) qrsh;
  (* Works very long *)
  (*  run show_int_list    (-1) qr  (REPR (fun q r   -> prog id_set3 bin_divo_list  (invoke "bin_divo"  (q ^~ (to_bin 8) ^~ (to_bin 4) ^. r))                 )) qrh;*)
  run show_int_list    (-1) q   (REPR (fun q     -> prog id_set3 bin_powo_list  (invoke "bin_powo"  ((to_bin 3) ^~ (to_bin 4) ^. q))                      )) qh;
  run show_int_list    (-1) q   (REPR (fun q     -> prog id_set3 bin_powo_list  (invoke "bin_powo"  ((to_bin 10) ^~ (to_bin 4) ^. q))                     )) qh;
  run show_int_list    (-1) qr  (REPR (fun q r   -> prog id_set3 bin_powo_list  (invoke "bin_powo"  (q ^~ r ^. (to_bin 1)))                               )) qrh;
  run show_int_list    (-1) q   (REPR (fun q     -> prog id_set3 bin_powo_list  (invoke "bin_powo"  ((to_bin 3) ^~ (to_bin 4) ^. q))                      )) qh;
  run show_int_list    (-1) q   (REPR (fun q     -> prog id_set3 bin_powo_list  (invoke "bin_powo"  (q          ^~ (to_bin 3) ^. (to_bin 125)))           )) qh;
  ()
