module L = List
open GT
open OCanren
open OCanren.Std
open Test.FO

let show_bool = show Bool.logic
let reify_bool = Bool.reify
let show_bool_list = show List.logic @@ show Bool.logic
let reify_bool_list = List.reify Bool.reify
let show_pair = show Pair.logic show_bool_list show_bool_list
let reify_pair = Pair.reify reify_bool_list reify_bool_list

let sep () =
  Printf.printf
    "======================================================================================================\n\n"
;;

let test ?(n = -1) ~show ~reifier (msg, g) =
  let stream = run q g (fun x -> x#reify reifier) in
  let answs = Stream.take ~n stream in
  Printf.printf "Test (%s):\n" msg;
  (match answs with
   | [] -> Printf.printf "  No answers\n"
   | _ -> L.iteri (fun i a -> Printf.printf "  Answer %.3d: %s\n" (i + 1) @@ show a) answs);
  Printf.printf "\n\n"
;;

let _ =
  test (REPR (fun q -> fresh p (head_is_true p q))) ~show:show_bool ~reifier:reify_bool;
  test
    (REPR (fun q -> fresh p (head_is_true (list Fun.id [ p ]) q)))
    ~show:show_bool
    ~reifier:reify_bool;
  test
    (REPR (fun q -> fresh p (head_is_true (list Fun.id [ q ]) p)))
    ~show:show_bool
    ~reifier:reify_bool;
  test
    (REPR (fun q -> head_is_true (list ( !! ) []) q))
    ~show:show_bool
    ~reifier:reify_bool;
  test
    (REPR (fun q -> head_is_true (list ( !! ) [ true ]) q))
    ~show:show_bool
    ~reifier:reify_bool;
  test
    (REPR (fun q -> head_is_true (list ( !! ) [ false ]) q))
    ~show:show_bool
    ~reifier:reify_bool;
  test
    (REPR (fun q -> head_is_true q !!true))
    ~show:show_bool_list
    ~reifier:reify_bool_list;
  test
    (REPR (fun q -> head_is_true q !!false))
    ~show:show_bool_list
    ~reifier:reify_bool_list;
  sep ()
;;

let _ =
  test
    (REPR (fun q -> forall q !!true))
    ~n:10
    ~show:show_bool_list
    ~reifier:reify_bool_list;
  test
    (REPR (fun q -> forall q !!false))
    ~n:10
    ~show:show_bool_list
    ~reifier:reify_bool_list;
  sep ()
;;

let _ =
  test
    (REPR (fun q -> same_lens q (list ( !! ) []) !!true))
    ~show:show_bool_list
    ~reifier:reify_bool_list;
  test
    (REPR (fun q -> same_lens q (list ( !! ) []) !!false))
    ~show:show_bool_list
    ~reifier:reify_bool_list;
  test
    (REPR (fun q -> same_lens q (list ( !! ) [ true ]) !!true))
    ~show:show_bool_list
    ~reifier:reify_bool_list;
  test
    (REPR (fun q -> same_lens q (list ( !! ) [ true ]) !!false))
    ~show:show_bool_list
    ~reifier:reify_bool_list;
  test
    (REPR (fun q -> same_lens q (list ( !! ) [ true; true ]) !!true))
    ~show:show_bool_list
    ~reifier:reify_bool_list;
  test
    (REPR (fun q -> same_lens q (list ( !! ) [ true; true ]) !!false))
    ~show:show_bool_list
    ~reifier:reify_bool_list;
  test
    (REPR (fun q -> fresh (a b) (q === pair a b) (same_lens a b !!true)))
    ~n:10
    ~show:show_pair
    ~reifier:reify_pair;
  test
    (REPR (fun q -> fresh (a b) (q === pair a b) (same_lens a b !!false)))
    ~n:10
    ~show:show_pair
    ~reifier:reify_pair;
  sep ()
;;

let _ =
  test
    (REPR (fun q -> eq_lists q (list ( !! ) []) !!true))
    ~show:show_bool_list
    ~reifier:reify_bool_list;
  test
    (REPR (fun q -> eq_lists q (list ( !! ) []) !!false))
    ~show:show_bool_list
    ~reifier:reify_bool_list;
  test
    (REPR (fun q -> eq_lists q (list ( !! ) [ true ]) !!true))
    ~show:show_bool_list
    ~reifier:reify_bool_list;
  test
    (REPR (fun q -> eq_lists q (list ( !! ) [ true ]) !!false))
    ~show:show_bool_list
    ~reifier:reify_bool_list;
  test
    (REPR (fun q -> eq_lists q (list ( !! ) [ true; true ]) !!true))
    ~show:show_bool_list
    ~reifier:reify_bool_list;
  test
    (REPR (fun q -> eq_lists q (list ( !! ) [ true; true ]) !!false))
    ~show:show_bool_list
    ~reifier:reify_bool_list;
  test
    (REPR (fun q -> fresh (a b) (q === pair a b) (eq_lists a b !!true)))
    ~n:10
    ~show:show_pair
    ~reifier:reify_pair;
  test
    (REPR (fun q -> fresh (a b) (q === pair a b) (eq_lists a b !!false)))
    ~n:10
    ~show:show_pair
    ~reifier:reify_pair;
  sep ()
;;

let _ =
  test (REPR (fun q -> matching q !!"1")) ~show:show_bool_list ~reifier:reify_bool_list;
  test (REPR (fun q -> matching q !!"2")) ~show:show_bool_list ~reifier:reify_bool_list;
  test (REPR (fun q -> matching q !!"3")) ~show:show_bool_list ~reifier:reify_bool_list;
  test (REPR (fun q -> matching q !!"4")) ~show:show_bool_list ~reifier:reify_bool_list;
  test (REPR (fun q -> matching q !!"5")) ~show:show_bool_list ~reifier:reify_bool_list;
  test (REPR (fun q -> matching q !!"6")) ~show:show_bool_list ~reifier:reify_bool_list;
  test (REPR (fun q -> matching q !!"7")) ~show:show_bool_list ~reifier:reify_bool_list;
  test (REPR (fun q -> matching q !!"8")) ~show:show_bool_list ~reifier:reify_bool_list;
  sep ()
;;

let matching_grounded x y =
  let is_bool x = conde [ x === !!true; x === !!false ] in
  fresh
    (a b c)
    (x === Std.list Fun.id [ a; b; c ])
    (is_bool a)
    (is_bool b)
    (is_bool c)
    (matching x y)
;;

let _ =
  test
    (REPR (fun q -> matching_grounded q !!"1"))
    ~show:show_bool_list
    ~reifier:reify_bool_list;
  test
    (REPR (fun q -> matching_grounded q !!"2"))
    ~show:show_bool_list
    ~reifier:reify_bool_list;
  test
    (REPR (fun q -> matching_grounded q !!"3"))
    ~show:show_bool_list
    ~reifier:reify_bool_list;
  test
    (REPR (fun q -> matching_grounded q !!"4"))
    ~show:show_bool_list
    ~reifier:reify_bool_list;
  test
    (REPR (fun q -> matching_grounded q !!"5"))
    ~show:show_bool_list
    ~reifier:reify_bool_list;
  test
    (REPR (fun q -> matching_grounded q !!"6"))
    ~show:show_bool_list
    ~reifier:reify_bool_list;
  test
    (REPR (fun q -> matching_grounded q !!"7"))
    ~show:show_bool_list
    ~reifier:reify_bool_list;
  test
    (REPR (fun q -> matching_grounded q !!"8"))
    ~show:show_bool_list
    ~reifier:reify_bool_list;
  sep ()
;;
