open Test_reexport
open Output.HO

let () =
  let open OCanren in
  let answers =
    run q (fun q -> good_a (( === ) q) !!true) (fun rr -> rr#reify OCanren.prj_exn)
    |> Stream.take
  in
  Stdlib.List.iter (fun x -> assert (Input.good_a x)) answers
;;

let rec show_mylist xs = GT.show Mylist.ground (GT.show Xxx.ground ()) show_mylist xs

let () =
  let open OCanren in
  let answers =
    run
      q
      (fun q -> good_list (( === ) q) !!true)
      (fun rr -> rr#reify (Reifier.fix (fun self -> Mylist.prj_exn OCanren.prj_exn self)))
    |> Stream.take
  in
  Stdlib.List.iter
    (fun x ->
      print_endline (show_mylist x);
      assert (Input.good_list x))
    answers
;;

open OCanren

module Nat = struct
  [%%distrib
  type nonrec 'a t = 'a gnat =
    | Z
    | S of 'a
  [@@deriving gt ~options:{ show; gmap }]

  type ground = ground t]
end

let () =
  let open OCanren in
  let answers =
    run q (fun q -> even_nat (( === ) q) !!true) (fun rr -> rr#reify Nat.prj_exn)
    |> Stream.take ~n:5
  in
  Stdlib.List.iter
    (fun x ->
      print_endline (GT.show Nat.ground x);
      assert (Input.even_nat (nat_from_ground x)))
    answers
;;
