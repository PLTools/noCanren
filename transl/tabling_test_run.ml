open MiniKanren
open MiniKanrenStd
open Tabling_test
open Tester

let rec int2num i = if i <= 0 then o () else s (int2num (i-1))

let show_bool x = if x then "true" else "false"

let show_number num =
  let rec helper = function
  | O   -> 0
  | S x -> 1  + (helper x)
  in
  string_of_int @@ helper num

let () =
 run_exn show_number (-1) q qh ("answers", (fun q -> (fib ((===)(int2num 23)) q )));
 
