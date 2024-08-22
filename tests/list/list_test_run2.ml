open OCanren
open OCanren.Std
open Tester
open List_test_unnesting

let () =
  let injected : int ilogic Std.List.injected = OCanren.Std.list ( !! ) [ 1 ] in
  let _ =
    OCanren.(run q)
      (list_map (fun x q17 -> x === q17) injected)
      (fun rr -> rr#reify (Std.List.reify OCanren.reify))
    |> OCanren.Stream.hd
  in
  ()
;;
