open Printf
open MiniKanren
open Tester.M
open ImplicitPrinters

let fst3 (x,_,_) = x

let goal_list a st =
  print_endline @@ State.show @@ fst3 st;
  (a === (embed "asdf")) st
let goal_int  a st =
  print_endline @@ State.show @@ fst3 st;
  (a === (embed 5)) st

let _ =
  let g = Logger.create () in
  Tester.M.run g
    (fun st ->
     let s1, v1 = q (fun q st -> goal_list q st, q) st in
     let (_:string logic) = v1 in
     print_endline @@ State.show @@ fst3 st;

     let s2, v2 = q (fun q st -> goal_int  q st, q) st in
     let (_:int logic) = v2 in
     print_endline @@ State.show @@ fst3 st;
     match take' ~n:1 s2 with
     | [s] ->
        let v: _ logic = fst @@ refine s v1 in
        let am_I_a_string  = to_value v in
        printf "Some result: %s.\n%!" (show_logic_naive v);
        printf "string: '%s'\n%!" am_I_a_string;
     | [ ] -> printf "No result.\n"
     | _ -> assert false
    )

(* open Tester *)

(* let _ = *)
(*   run empty_reifier  6  q (fun q   st -> REPR (fives    q st), ["q", q]); *)
(*   run empty_reifier  1  q (fun q   st -> REPR (a_and_b  q st), ["q", q]); *)
(*   run empty_reifier  2  q (fun q   st -> REPR (a_and_b' q st), ["q", q]); *)
(*   run empty_reifier  1  q (fun q   st -> REPR (appendo q (of_list [3; 4]) (of_list [1; 2; 3; 4]) st), ["q", q]); *)
(*   run  empty_reifier  4 qr (fun q r st -> REPR (appendo q (of_list ([]:int list) ) r                          st), ["q", q; "r", r]); *)
(*   run  empty_reifier  1  q (fun q   st -> REPR (reverso q (of_list [1; 2; 3; 4])                  st), ["q", q]); *)
(*   (\* run  empty_reifier  1  q (fun q   st -> REPR (reverso (of_list []) (of_list [])                 st), ["q", q]); *\) *)
(*   run  empty_reifier  1  q (fun q   st -> REPR (reverso (of_list [1; 2; 3; 4]) q                  st), ["q", q]); *)
(*   run  empty_reifier  1  q (fun q   st -> REPR (reverso q q                                       st), ["q", q]); *)
(*   run  empty_reifier  2  q (fun q   st -> REPR (reverso q q                                       st), ["q", q]); *)
(*   run  empty_reifier  3  q (fun q   st -> REPR (reverso q q                                       st), ["q", q]); *)
(*   run  empty_reifier 10  q (fun q   st -> REPR (reverso q q                                       st), ["q", q]); *)
(*   run  empty_reifier  2  q (fun q   st -> REPR (reverso q (of_list [1])                           st), ["q", q]); *)
(*   run  empty_reifier  1  q (fun q   st -> REPR (reverso (of_list [1]) q                           st), ["q", q]); *)
(*   () *)
