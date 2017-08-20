open GT
open MiniKanren
open MiniKanren.Deep
open Tester
open Tester.Deep

(*** Just goals ***)

let id_set0 = IdentSet.empty ()

let a   : int logic = IdentSet.new_ident id_set0 0
let b   : int logic = IdentSet.new_ident id_set0 1
let c   : int logic = IdentSet.new_ident id_set0 2


let just_a = def "just_a" (!^ a)
  (a === !!5)

let a_and_b = def "a_and_b" (!^ a) (
  fresh (!^ b) [
    (a === !!7);
    (disj (b === !!6) (b === !!5))
  ]
)

let a_and_b' = def "a_and_b'" (!^ b) (
  fresh (!^ a) [
    (a === !!7);
    (disj (b === !!6) (b === !!5))
  ]
)

let fives = def "fives" (!^ a) (
  disj (a === !!5) (invoke "fives" (!^ a))
)

let show_int = GT.(show logic @@ show int)

let () =
  run show_int   (-1) q   (REPR (fun q     -> prog id_set0 [just_a  ]     (invoke "just_a"    (!^ q))   )) qh;
  run show_int   (-1) q   (REPR (fun q     -> prog id_set0 [a_and_b ]     (invoke "a_and_b"   (!^ q))   )) qh;
  run show_int   (-1) q   (REPR (fun q     -> prog id_set0 [a_and_b']     (invoke "a_and_b'"  (!^ q))   )) qh;
  run show_int   (10) q   (REPR (fun q     -> prog id_set0 [fives]        (invoke "fives"     (!^ q))   )) qh;
  ()

