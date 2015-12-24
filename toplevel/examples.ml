(** Declare a and b *)
let a_and_b a =
  call_fresh (fun b ->
      conj (a === !7)
           (disj (b === !6)
                 (b === !5)
           )
  )

(** Eval a and b *)
let _ = run show_int empty_reifier 1 q (fun q st -> a_and_b q st, ["q", q])

(** Declare appendo *)
let rec appendo a b ab =
  disj
    (conj (a === !Nil) (b === ab) )
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
    (conj (a === !Nil) (b === !Nil))
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
  run show_int_list empty_reifier 1 q
      (fun q st -> (appendo q (of_list [3; 4]) (of_list [1; 2; 3; 4]) st), ["q", q])

(** Run appendo 2 *)
let _ =
  run show_int_list empty_reifier  4 qr
      (fun q r st -> (appendo q (of_list []) r st), ["q", q; "r", r])

(** Run reverso 1 *)
let _ =
  run show_int_list empty_reifier 1 q
      (fun q st -> (reverso q (of_list [1; 2; 3; 4]) st), ["q", q])

(** Declare fives *)
let rec fives x = (x === !5) ||| defer (fives x)

(** Run fives *)
let _ =
  run show_int empty_reifier 10 q (fun q st -> (fives q st), ["q", q])
