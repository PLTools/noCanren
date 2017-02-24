open MiniKanren
open Tester

let f1 state =
  print_endline "f1";
  failure state
let f2 state =
  print_endline "f2";
  failure state
let f3 state =
  print_endline "f3";
  failure state

let () =
  run_exn string_of_int (-1) q qh (REPR(fun q -> (q=== inj_int 1) &&& conde [ f1; f2; f3 ]))
