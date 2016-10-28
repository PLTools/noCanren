open Numero_decls
open MiniKanren
open Tester

let _ =
  (* run show_int_list  22  qrs (REPR (fun q r s   -> pluso q r s                                      ))  qrsh;
  run show_int_list (-1)  qr (REPR (fun q r     -> multo q r (build_num 1)                          ))   qrh;
  run show_int_list (-1)   q (REPR (fun q       -> multo (build_num 7) (build_num 63) q             ))    qh;
  run show_int_list (-1)  qr (REPR (fun q r     -> divo (build_num 3) (build_num 2) q r             ))   qrh;
  run show_int_list  34  qrs (REPR (fun q r s   -> multo q r s                                      ))  qrsh;
  run show_int_list  10   qr (REPR (fun q r     -> test17 q r                                       ))   qrh;
  run show_int_list  15   qr (REPR (fun q r     -> lelo q r                                         ))   qrh;
  run show_int_list (-1)   q (REPR (fun q       -> lto q (build_num 5)                              ))    qh;
  run show_int_list (-1)   q (REPR (fun q       -> lto (build_num 5) q                              ))    qh;
  run show_int_list   6 qrst (REPR (fun q r s t -> divo q r s t                                     )) qrsth;
  run show_int_list (-1)   q (REPR (fun q       -> logo (build_num 14) (build_num 2) (build_num 3) q))    qh; *)
  (* run show_int_list   5  qrs (REPR (fun q r s   -> test27 q r s                                     ))  qrsh; *)
  run show_int_list (-1)   q (REPR (fun q       -> expo (build_num 3) (build_num 2) q               ))    qh;
