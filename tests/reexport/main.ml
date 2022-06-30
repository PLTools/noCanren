open Test_reexport
open Output

let () =
  let open OCanren in
  let answers =
    run
      q
      (fun q -> Output.good_a_o (( === ) q) !!true)
      (fun rr -> rr#reify OCanren.prj_exn)
    |> Stream.take
  in
  List.iter (fun x -> assert (Input.good_a x)) answers
;;

let fmapt fa fb subj =
  let open OCanren.Env.Monad in
  OCanren.Env.Monad.return (GT.gmap Output.glist) <*> fa <*> fb <*> subj
;;

let mylist_prj_exn ra =
  let open OCanren in
  let open Env.Monad in
  Reifier.fix (fun self -> OCanren.prj_exn <..> chain (fmapt ra self))
;;

let rec show_mylist xs = GT.show glist (GT.show gxxx ()) show_mylist xs

let () =
  let open OCanren in
  let answers =
    run
      q
      (fun q -> Output.good_list_o (( === ) q) !!true)
      (fun rr -> rr#reify (mylist_prj_exn OCanren.prj_exn))
    |> Stream.take
  in
  List.iter
    (fun x ->
      print_endline (show_mylist x);
      assert (Input.good_list x))
    answers
;;
