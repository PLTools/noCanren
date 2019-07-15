open GT
open MiniKanren
open Tester
open Stlc
open GLam

let match_lam a onVar onApp onAbs = cont_delay @@
  conde [
    Fresh.one (fun x ->
      (a === (v x)) <&>
      (onVar x)
    );
    Fresh.two (fun p q ->
      (a === (app p q)) <&>
      (onApp p q)
    );
    Fresh.two (fun x l ->
      (a === (abs x l)) <&>
      (onAbs x l)
    );
  ]

let rec substo l x a l' = cont_delay @@
  match_lam l
    (fun y   -> (x === y) <&> (l' === a))
    (fun p q -> Fresh.two (fun p' q' ->
                  (l' === (app p' q')) <&>
                  (substo p x a p') <&>
                  (substo q x a q')
                )
    )
    (fun v b -> conde [(x === v) <&> (l' === l);
                       Fresh.one (fun b' ->
                         (l' === (abs v b')) <&>
                         (substo b x a b')
                       )])

let rec evalo m n = cont_delay @@
  match_lam m
    (fun _ -> n === m)
    (fun f a ->
       Fresh.two (fun f' a' ->
         (match_lam f'
            (fun _   -> n === (app f' a'))
            (fun _ _ -> n === (app f' a'))
            (fun x l -> Fresh.one (fun l' ->
                          (substo l x a' l') <&>
                          (evalo l' n)
                        ))
         ) <&>
         (evalo f f') <&>
         (evalo a a')
       )
    )
    (fun _ _ -> n === m)

let _ =
  run_exn show_rlam (-1)    q   qh (REPR (fun q   -> substo (v varX) varX (v varY) q                   ));
  run_exn show_rlam (-1)    q   qh (REPR (fun q   -> evalo (abs varX (v varX)) q                       ));
  run_exn show_rlam (1)    q   qh (REPR (fun q   -> evalo (app (abs varX (v varX)) (v varY)) q        ));

  run_exn show_rlam 1    q   qh (REPR (fun q   -> evalo (app (abs varX (v varX)) q)        (v varY) ));
  run_exn show_rlam 1    q   qh (REPR (fun q   -> evalo (app (abs varX q)        (v varY)) (v varY) ));

  run_exn show_rlam (-1)    q   qh (REPR (fun q   -> evalo (app (v varX)            (v varX)) q        ));
  run_exn show_rlam (-1)    q   qh (REPR (fun q   -> evalo (v varX) q                                  ));
  ()

let runL n = runR glam_reifier show_rlam show_llam n

let _ =
  runL 1   q   qh (REPR (fun q   -> evalo (app q (v varX)) (v varX)               ));
  runL 1  qr  qrh (REPR (fun q r -> evalo (app r q)        (v varX)               ));
  ()
