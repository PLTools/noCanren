open MiniKanren
open Logic


let f = conj (var !!"x") (disj (var !!"y") (not_ (var !!"z")))

let dict x y z n = cons !!"y" y @@ cons !!"x" x @@ cons n  z (empty())

let print x =
  print_endline (match x with Nothing -> "N" | Just v -> if v then "1" else "0")

let () =
  let l = [false; true] in
  List.iter (fun x -> List.iter (fun y -> List.iter (fun z -> dict x y z "z" |> calc_left f |> print) l) l) l;
  print_string "\n";
  List.iter (fun x -> List.iter (fun y -> List.iter (fun z -> dict x y z "a" |> calc_left f |> print) l) l) l;
  print_string "\n";
  List.iter (fun x -> List.iter (fun y -> List.iter (fun z -> dict x y z "z" |> calc_both f |> print) l) l) l;
  print_string "\n";
  List.iter (fun x -> List.iter (fun y -> List.iter (fun z -> dict x y z "a" |> calc_both f |> print) l) l) l

