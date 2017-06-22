open MiniKanren
open Tester
open Printf
open GT

let ilist xs = inj_list @@ List.map (!!) xs
let just_a a = a === !!5

let a_and_b a =
  fresh (b)
    (a === !!7)
    (conde  [ b === !!6
            ; b === !!5
            ])

let a_and_b' b =
  fresh (a)
    (a === !!7)
    (conde  [ (b === !!6)
            ; (b === !!5)
            ])


let rec fives x =
  conde
    [ (x === !!5)
    ; (delay @@ fun () -> fives x)
    ]

let show_int       = show(int)
let show_int_list  = (show(List.ground) (show int))
let show_intl_list = (show(List.logic ) (show(logic) (show int)))
let show_logic_list h xs = show_intl_list @@ List.reify ManualReifiers.int_reifier h xs

let rec appendo a b ab =
  (* let (===) ?loc = unitrace ?loc show_logic_list in *)
  conde
    [ (*project3 "appendo simple (a,b,ab): " show_logic_list a b ab &&& *)
      ((a === nil ()) &&& (b === ab))
    ; fresh (h t ab')
          (* project3 "appendo complex (a,b,ab): " show_logic_list a b ab &&& *)
          (a === h%t)
          (h%ab' === ab)
          (appendo t b ab')
    ]

let runL n         = runR (List.reify ManualReifiers.int_reifier) show_int_list show_intl_list n

let rec reverso a b =
  (* let (===) = unitrace (fun h t -> GT.(show List.logic @@ show logic (show int)) @@
    List.reify ManualReifiers.int_reifier h t)
  in *)
  conde
    [ ((a === nil ()) &&& (b === nil ()))
    ; fresh (h t a')
        (a === h%t)
        (appendo a' !<h b)
        (reverso t a')
    ]


let _ =
  run_exn show_int_list  1  q qh (REPR (fun q   -> appendo q (ilist [3; 4]) (ilist [1; 2; 3; 4])   ));
  run_exn show_int_list  1  q qh (REPR (fun q   -> reverso q (ilist [1; 2; 3; 4])                  ));
  run_exn show_int_list  1  q qh (REPR (fun q   -> reverso (ilist [1; 2; 3; 4]) q                  ));
  run_exn show_int_list  2  q qh (REPR (fun q   -> reverso q (ilist [1])                           ));
  run_exn show_int_list  1  q qh (REPR (fun q   -> reverso (ilist [1]) q                           ));
  run_exn show_int       1  q qh (REPR (fun q   -> a_and_b q                                       ));
  run_exn show_int       2  q qh (REPR (fun q   -> a_and_b' q                                      ));
  run_exn show_int      10  q qh (REPR (fun q   -> fives q                                         ));
  ()

let _withFree =
  runL          1  q  qh (REPR (fun q   -> reverso (ilist []) (ilist [])                ));
  runL          2  q  qh (REPR (fun q   -> reverso q q                                  ));
  runL          4 qr qrh (REPR (fun q r -> appendo q (ilist []) r                       ));
  runL          1  q  qh (REPR (fun q   -> reverso q q                                  ));
  runL          2  q  qh (REPR (fun q   -> reverso q q                                  ));
  runL          3  q  qh (REPR (fun q   -> reverso q q                                  ));
  runL         10  q  qh (REPR (fun q   -> reverso q q                                  ));
  ()
