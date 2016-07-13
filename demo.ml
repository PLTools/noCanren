open MiniKanren
open Tester.M
open ImplicitPrinters

let just_a a = a === (embed 5)

let a_and_b a =
  call_fresh_named "b" (fun b ->
      conj (a === embed 7)
           (disj (b === embed 6)
                 (b === embed 5)
           )
  )

let a_and_b' b =
  call_fresh (fun a ->
      conj (a === embed 7)
           (disj (b === embed 6)
                 (b === embed 5)
           )
  )

let rec fives x =
  disj (x === embed 5)
       (delay_goal (fun () -> fives x))

let rec appendo a b ab =
  disj
    (conj (a === llist_nil) (b === ab) )
    (call_fresh (fun h ->
      (call_fresh (fun t ->
        (conj (a === h % t)
           (call_fresh (fun ab' ->
              conj (h % ab' === ab)
                   (appendo t b ab')
           ))
      )))
    ))

let rec reverso a b =
  disj
    (conj (a === llist_nil) (b === llist_nil))
    (call_fresh (fun h ->
      (call_fresh (fun t ->
          (conj (a === h % t)
                (call_fresh (fun a' ->
                   conj (appendo a' !< h b)
                        (reverso t a')
                ))
        )
    )
    )))

let (!) = inj
let test_diseq1 x y =
  disj
    ((x === !1) &&& (y =/= !5))
    ((x === !2) &&& (y =/= !6))

open Tester

let _ =
  run2 ~n:2 (REPR test_diseq1);

  (* run1 ~n:1  (REPR(fun q -> reverso (of_list ([]: int list)) (of_list ([]: int list)))); *)
  (* run1 ~n:10 (REPR(fun q -> reverso q q) ); *)


  ()
