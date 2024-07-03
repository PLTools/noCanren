open OCanren
open Btv.FO

let rec print_answers ~pp ppf stream =
  let answ, stream = Stream.retrieve ~n:1 stream in
  match answ with
  | [] -> ()
  | [ a ] ->
    Format.fprintf ppf "%a\n%!" pp a;
    print_answers ~pp ppf stream
  | _ -> assert false
;;

let _ =
  run
    q
    (fun q -> verify q !!true)
    (fun rr ->
      rr#reify
        (Std.List.prj_exn
           (Std.List.prj_exn (fun x y -> Std.Nat.to_int @@ Std.Nat.prj_exn x y))))
  |> Format.printf "%a" (print_answers ~pp:[%fmt: GT.int GT.list GT.list])
;;
