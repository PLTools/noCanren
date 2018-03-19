open MiniKanren
open Test4



(* let print x =
 *   print_endline (match x with Nothing -> "N" | Just v -> if v then "1" else "0") *)

(* let () =
 *   let l = [false; true] in
 *   List.iter (fun x -> List.iter (fun y -> List.iter (fun z -> dict x y z "z" |> calc_left f |> print) l) l) l;
 *   print_string "\n";
 *   List.iter (fun x -> List.iter (fun y -> List.iter (fun z -> dict x y z "a" |> calc_left f |> print) l) l) l;
 *   print_string "\n";
 *   List.iter (fun x -> List.iter (fun y -> List.iter (fun z -> dict x y z "z" |> calc_both f |> print) l) l) l;
 *   print_string "\n";
 *   List.iter (fun x -> List.iter (fun y -> List.iter (fun z -> dict x y z "a" |> calc_both f |> print) l) l) l *)
