open MiniKanren
open Tester
open Printf
open GT

let ilist xs = inj_list (!!) xs
let just_a a = a === !!5

let rec appendo a b ab =
  conde
    [ ((a === nil ()) &&& (b === ab))
    ; Fresh.three @@ fun h t ab' ->
        (a === h%t) &&&
        (h%ab' === ab) &&&
        (appendo t b ab') 

    ]

let rec reverso a b =
  conde
    [ ((a === nil ()) &&& (b === nil ()))
    ; Fresh.three @@ fun h t a' ->
        (a === h%t) &&&
        (appendo a' !<h b) &&&
        (reverso t a') 
    ]

let show_int       = show(int)
let show_int_list  = (show(List.ground) (show int))
let show_intl_list = (show(List.logic ) (show(logic) (show int)))
let runL n         = runR (List.reify ManualReifiers.int) show_int_list show_intl_list n

let _ =
  run_exn show_int_list  1  q qh ("", (fun q   -> appendo q (ilist [3; 4]) (ilist [1; 2; 3; 4])   ));
  run_exn show_int_list  1  q qh ("", (fun q   -> reverso q (ilist [1; 2; 3; 4])                  ));
  run_exn show_int_list  1  q qh ("", (fun q   -> reverso (ilist [1; 2; 3; 4]) q                  ));
  run_exn show_int_list  2  q qh ("", (fun q   -> reverso q (ilist [1])                           ));
  run_exn show_int_list  1  q qh ("", (fun q   -> reverso (ilist [1]) q                           ));
  ()

let _withFree =
  runL          1  q  qh ("", (fun q   -> reverso (ilist []) (ilist [])                ));
  runL          2  q  qh ("", (fun q   -> reverso q q                                  ));
  runL          4 qr qrh ("", (fun q r -> appendo q (ilist []) r                       ));
  runL          1  q  qh ("", (fun q   -> reverso q q                                  ));
  runL          2  q  qh ("", (fun q   -> reverso q q                                  ));
  runL          3  q  qh ("", (fun q   -> reverso q q                                  ));
  runL         10  q  qh ("", (fun q   -> reverso q q                                  ));
  ()
