(** Declare a and b *)
let a_and_b a =
  call_fresh (fun b ->
      conj (a === 7)
           (disj (b === 6)
                 (b === 5)
           )
  )

(** Eval a and b *)
let _ = run (mkshow int)  1 q  (fun q st -> a_and_b q st        , ["q", q])

(** Declare appendo *)
let rec appendo a b ab =
  disj
    (conj (a === []) (b === ab) )
    (call_fresh (fun h ->
      (call_fresh (fun t ->
        (conj (a === h::t)
           (call_fresh (fun ab' ->
              conj (h::ab' === ab)
                   (appendo t b ab')
           ))
      )))
    ))

(** Declare reverso *)
let rec reverso a b =
  disj
    (conj (a === []) (b === []))
    (call_fresh (fun h ->
      (call_fresh (fun t ->
          (conj (a === h::t)
              (call_fresh (fun a' ->
                 conj (appendo a' [h] b)
                      (reverso t a')
              ))
        )
    )
      )))


(** Run appendo 1 *)
let _ = run int_list       1 q  (fun q   st -> (appendo q [3; 4] [1; 2; 3; 4] st), ["q", q])

(** Run appendo 2 *)
let _ = run int_list       4 qp (fun q p st ->  (appendo q [] p st)               , ["q", q; "p", p])

(** Run reverso 1 *)
let _ = run int_list       1 q  (fun q   st ->  (reverso q [1; 2; 3; 4] st)       , ["q", q])
