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

let nilo a = (a === llist_nil)
let seveno x = (x === embed 7)
let wtfo xs n = conj (nilo xs) (seveno n)

open Tester

let _ =
  run1 ~n:5 (REPR fives);
  run1 ~n:1 (REPR a_and_b);
  run1 ~n:1 (REPR a_and_b');

  run1 ~n:1 (REPR(fun q -> appendo q (of_list [3;4]) (of_list [1;2;3;4]) ) );
  run2 ~n:4 (REPR(fun q r -> appendo q (of_list ([]: int list)) r) );
  run2 ~n:4 (REPR(fun q r -> appendo (of_list [1]) q r) );

  run1 ~n:1  (REPR(fun q -> reverso q (of_list [1; 2; 3; 4]) ) );
  run1 ~n:1  (REPR(fun q -> reverso q q) );
  run1 ~n:2  (REPR(fun q -> reverso q q) );
  run1 ~n:5  (REPR(fun q -> reverso q q) );

  run1 ~n:1  (REPR(fun q -> reverso q (of_list [1]) ) );
  run1 ~n:1  (REPR(fun q -> reverso (of_list [1]) q ) );

  (* run 1   q ~varnames:["q"]     (REPR a_and_b); *)
  (* run 2   q ~varnames:["q"]     (REPR a_and_b'); *)
  (* run 1   q ~varnames:["q"]     (REPR (fun q   -> appendo q (of_list [3; 4]) (of_list [1; 2; 3; 4])) ); *)
  (* run 4  qr ~varnames:["q"]     (REPR (fun q r -> appendo q (of_list ([]:int list)) r) ); *)

  (* run 1   q ~varnames:["q"]     (REPR (fun q   -> reverso q (of_list [1; 2; 3; 4])) ); *)
  (* run 1   q ~varnames:["q"]     (REPR (fun q   -> reverso (of_list [1; 2; 3; 4]) q) ); *)
  (* run 1   q ~varnames:["q"]     (REPR (fun q   -> reverso (of_list ([]: int list)) (of_list ([]: int list)) )); *)
  (* run 1   q ~varnames:["q"]     (REPR (fun q   -> reverso q q                   )); *)
  (* run 2   q ~varnames:["q"]     (REPR (fun q   -> reverso q q                   )); *)
  (* run 10  q ~varnames:["q"]     (REPR (fun q   -> reverso q q                   )); *)
  (* run 2   q ~varnames:["q"]     (REPR (fun q   -> reverso q (of_list [1])       )); *)
  (* run 1   q ~varnames:["q"]     (REPR (fun q   -> reverso (of_list [1]) q       )); *)
  ()
