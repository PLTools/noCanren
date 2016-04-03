(** Declare a and b *)
let a_and_b a =
  call_fresh (fun b ->
      conj (a === embed 7)
           (disj (b === embed 6)
                 (b === embed 5)
           )
  )

(** Eval a and b *)
let _ =   run 1 one ~varnames:["q"]     (REPR a_and_b)

(** Declare appendo *)
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

(** Declare reverso *)
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


(** Run appendo 1 *)
let _ =
  run 1 (succ one) ~varnames:["q"] (REPR (fun q -> appendo q (of_list [3; 4]) (of_list [1; 2; 3; 4])) )

(** Run appendo 2 *)
let _ =
  run 4 (succ one) ~varnames:["q"; "r"] (REPR (fun q r -> appendo q llist_nil r)

(** Run reverso 1 *)
let _ =
  run 1 one ~varnames:["q"] (REPR (fun q -> (reverso q (of_list [1; 2; 3; 4]))) )

(** Declare fives *)
let rec fives x = (x === embed 5) ||| delay_goal (fun () -> fives x)

(** Run fives *)
let _ =
  run 1 one ~varnames:["q"]     (REPR fives)
