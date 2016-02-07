open MiniKanren
open Tester
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

let run  = Convenience.run
let run1 = Convenience.run1


let _ =
  (* run1  1 (REPR fives); *)
  run 1 one (REPR (fun q -> fives q) );
  (* run empty_reifier  1  q (fun q   -> REPR (a_and_b  q ), ["q", q]); *)
  (* run empty_reifier  2  q (fun q   -> REPR (a_and_b' q ), ["q", q]); *)
  (* run empty_reifier  1  q (fun q   -> REPR (appendo q (of_list [3; 4]) (of_list [1; 2; 3; 4]) st), ["q", q]); *)
  (* run  empty_reifier  4 qr (fun q r st -> REPR (appendo q (of_list ([]:int list) ) r                          st), ["q", q; "r", r]); *)
  (* run  empty_reifier  1  q (fun q   st -> REPR (reverso q (of_list [1; 2; 3; 4])                  st), ["q", q]); *)
  (* run  empty_reifier  1  q (fun q   st -> REPR (reverso (of_list []) (of_list [])                 st), ["q", q]); *)
  (* run  empty_reifier  1  q (fun q   st -> REPR (reverso (of_list [1; 2; 3; 4]) q                  st), ["q", q]); *)
  (* run  empty_reifier  1  q (fun q   st -> REPR (reverso q q                                       st), ["q", q]); *)
  (* run  empty_reifier  2  q (fun q   st -> REPR (reverso q q                                       st), ["q", q]); *)
  (* run  empty_reifier  3  q (fun q   st -> REPR (reverso q q                                       st), ["q", q]); *)
  (* run  empty_reifier 10  q (fun q   st -> REPR (reverso q q                                       st), ["q", q]); *)
  (* run  empty_reifier  2  q (fun q   st -> REPR (reverso q (of_list [1])                           st), ["q", q]); *)
  (* run  empty_reifier  1  q (fun q   st -> REPR (reverso (of_list [1]) q                           st), ["q", q]); *)
  ()
