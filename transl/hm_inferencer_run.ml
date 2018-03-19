open MiniKanren
open Hm_inferencer
open Tester


let show_maybe f = function
  | Nothing -> "None"
  | Just x -> Printf.sprintf "Some (%s)" (f x)

let rec show_gnum num =
  let rec helper = function
  | Z -> 0
  | S x -> 1  + (helper x)
  in
  string_of_int @@ helper num

let rec show_lambda_type f = function
  | TInt -> "int"
  | TBool -> "bool"
  | TVar n -> Printf.sprintf "_.(%s)" (f n)
  | TFun (l,r) -> Printf.sprintf "(%s -> %s)" (show_lambda_type f l) (show_lambda_type f r)


(* let (_:int) = the_true *)
let myshow x = show_maybe (show_lambda_type show_gnum) x

let () =
  (* run_exn (fun _ -> ":)") (-1) q  qh  ("true?", (fun q -> the_true q  ) ); *)
  ()

let () =
  run_exn myshow (-1) q  qh  ("typeof term1", (fun q -> nat_type_inference term2 q  ) );
  ()
