open MiniKanren
open MiniKanrenStd
open Tabling_test
open Tester

let rec int2num i = if i <= 0 then o () else s (int2num (i-1))

let show_bool x = if x then "true" else "false"

let () =
 run_exn show_bool (-1) q qh ("answers", (fun q -> (fib ((===)(int2num 100)) q )));
 run_exn show_bool (-1) q qh ("answers", (fun q -> (fib ((===)(int2num 200)) q )));
 
