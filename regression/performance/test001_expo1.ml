open Numero_decls
open MiniKanren
open Tester

let _ =
  run show_int_list (-1)   q (REPR (fun q       -> expo (build_num 3) (build_num 5) q )) qh;
  ()
