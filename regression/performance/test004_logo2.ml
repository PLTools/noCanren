open Numero_decls
open MiniKanren
open Tester

let _ =
  run show_int_list (-1)   q (REPR (fun q       -> logo (build_num 1025) (build_num 2) q (build_num 1) ))    qh;
  ()
