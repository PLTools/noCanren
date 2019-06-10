open MiniKanren
open Tester
open Stlc

open GLam

let rec substo l x a l' = cont_delay @@
  conde [
    (l === v x) <&> (l' === a);
    Fresh.four (fun m n m' n' ->
      (l  === app m n) <&>
      (l' === app m' n') <&>
      (substo m x a m') <&>
      (substo n x a n')
    );
    Fresh.two (fun v b ->
      (l === abs v b) <&>
      (conde [
        (x  === v) <&> (l' === l);
        Fresh.one (fun b' ->
          (l' === abs v b') <&> (substo b x a b')
        )
      ])
    )
  ]

let rec evalo m n = cont_delay @@
  conde [
    Fresh.one (fun x ->
      (m === v x) <&>
      (n === m)
    );
    Fresh.two (fun x l ->
      (m === abs x l) <&>
      (n === m)
    );
    Fresh.four (fun f a f' a' ->
      (m === app f a) <&>
      (evalo f f') <&>
      (evalo a a') <&>
      (conde [
        Fresh.three (fun x l l' ->
          (f' === abs x l) <&>
          (substo l x a' l') <&>
          (evalo l' n)
        );
        Fresh.two (fun p q ->
          (f' === app p q) <&> (n === app f' a')
        );
        Fresh.one (fun x ->
          (f' === v x) <&> (n === app f' a')
        )
       ])
      )
  ]

let a_la_quine q r s =
  evalo (app q r) s <&> evalo (app r s) q <&> evalo (app s q) r

let _ =
  run_exn show_rlam (-1)    q   qh (REPR (fun q   -> substo (v varX) varX (v varY) q                       ));
  run_exn show_rlam (-1)    q   qh (REPR (fun q   -> evalo (abs varX (v varX)) q                           ));
  (* run_exn show_rlam 2    q   qh (REPR (fun q   -> evalo (abs varX (v varX)) q                           )); *)
  run_exn show_rlam (-1)    q   qh (REPR (fun q   -> evalo (app (abs varX (v varX)) (v varY))        q     ));

  run_exn show_rlam 1    q   qh (REPR (fun q   -> evalo (app (abs varX (v varX))        q) (v varY)     ));
  run_exn show_rlam 1    q   qh (REPR (fun q   -> evalo (app (abs varX        q) (v varY)) (v varY)     ));

  run_exn show_rlam (-1)    q   qh (REPR (fun q   -> evalo (app            (v varX) (v varX)) q            ));
  run_exn show_rlam (-1)    q   qh (REPR (fun q   -> evalo (v varX)    q                                   ))

let runL n = runR glam_reifier show_rlam show_llam n

let _withFree =
  runL 1     q   qh (REPR (fun q     -> evalo (app q (v varX)) (v varX)             ));
  runL 1    qr  qrh (REPR (fun q r   -> evalo (app r q)        (v varX)             ));
  runL 2    qrs qrsh (REPR (fun q r s -> a_la_quine q r s                            ))
