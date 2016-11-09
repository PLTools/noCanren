open MiniKanren
open Tester
open Quine_decls

let _ =
  (* run show_int_list (-1)   q (REPR (fun q       -> expo (build_num 3) (build_num 5) q )) qh; *)
  (* find_quines 5; *)
  find_twines 2;
  ()
