open MiniKanren
open Tester.M
open ImplicitPrinters

let just_a a = a === (embed 5)


let nilo a = (a === llist_nil)
let seveno x = (x === embed 7)
let wtfo xs n = conj (nilo xs) (seveno n)

let diseq1 a b =
  ((a === (embed 3)) &&& (a =/= b))

let diseq2 a b =
   ((a === (embed 3)) &&& (a =/= b)) &&& ( (b=== embed 4)  )

let diseq3 a b c =
  ((a === (embed 3)) &&& (a =/= b) &&& (c =/= b))

open Tester

let _ =
  (* run2 ~n:2 (REPR diseq1); *)
  (* run3 ~n:2 (REPR diseq3); *)
  run2 ~n:1 (REPR diseq2);
  ()
