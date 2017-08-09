open MiniKanren
open Tester
open Printf
open GT

let show_int       = show(int)
let show_int_list  = (show(List.ground) (show int))
let show_intl_list = (show(List.logic ) (show(logic) (show int)))
let runL n         = runR (List.reify ManualReifiers.int) show_int_list show_intl_list n

let ilist xs = inj_list (!!) xs
let just_a a = a === !!5
let project3 ~msg x y z = project3 ~msg (fun h t -> show_intl_list @@ List.reify ManualReifiers.int h t) x y z
let project2 ~msg x y   = project2 ~msg (fun h t -> show_intl_list @@ List.reify ManualReifiers.int h t) x y

effect AskAppendoCache : (Obj.t * Obj.t * Obj.t) -> Cache3.t
effect AskReversoCache : (Obj.t * Obj.t * Obj.t) -> Cache3.t

let rec appendo a b ab = fun st ->
  let _ = ignore @@ project3 ~msg:"Entering appendo" a b ab st in
  let cache =
    let arg = Obj.(repr a, repr b, repr ab) in
    try perform (AskAppendoCache arg)
    with Unhandled -> Cache3.(extend arg empty)
  in

  let appendo a b ab = fun st ->
    match appendo a b ab st with
    | ss -> ss
    | effect (AskAppendoCache new_arg) k ->
        continue k Cache3.(extend new_arg cache)
  in
  conde
    [ ((a === nil ()) &&& (b === ab))
    ; Fresh.three @@ fun h t ab' ->
        ?& [
          (a === h%t);
          (h%ab' === ab);
          (appendo t b ab');
        ]
    ]
    st

let rec reverso a b = fun st ->
  let _ = ignore @@ project2 ~msg:"Entering reverso" a b st in
  let cache =
    let arg = Obj.(repr a, repr b, repr 1) in
    try perform (AskReversoCache arg)
    with Unhandled -> Cache3.(extend arg empty)
  in
  let reverso a b = fun st ->
    match reverso a b st with
    | ss -> ss
    | effect (AskReversoCache new_arg) k ->
        continue k Cache3.(extend new_arg cache)
  in
  conde
    [ ((a === nil ()) &&& (b === nil ()))
    ; Fresh.three @@ fun h t a' ->
        (a === h%t) &&&
        (reverso t a') &&&
        (appendo a' !<h b)
    ]
    st


let () =
  runL (-1)  q  qh ("", (fun q   -> appendo (ilist [1;2;3]) (ilist [4;5]) q       ));
  runL (-1)  q  qh ("", (fun q   -> reverso (ilist [1;2;3]) q                     ));
()

(*
exception RelDivergeExn
let par_conj_exn f g st =
  let (get_stream, next) =
    try let stream = f st in
        (fun () -> stream), g
    with RelDivergeExn -> (fun () -> g st), f
  in
  let stream =
    try get_stream ()
    with RelDivergeExn ->
      print_endline "both streams in par_conj_exn has been diverged";
      failure st
  in
  MKStream.bind stream next

type ask_result = Hang | Later of Cache3.t;;
effect AskAppendoCache : (Obj.t * Obj.t * Obj.t) * State.t -> ask_result
effect AskReversoCache : (Obj.t * Obj.t * Obj.t) * State.t -> ask_result

let rec appendo a b ab = fun st ->
  let _ = ignore @@ project3 ~msg:"Entering appendo" a b ab st in
  match perform (AskAppendoCache (Obj.(repr a, repr b, repr ab), st)) with
  | Hang -> raise RelDivergeExn
  | Later cache ->
      let () = printf "Size of cache is %d\n%!" (Cache3.size cache) in
      let appendo x y xy st =
        match appendo x y xy st with
        | x -> x
        | effect (AskAppendoCache (new_arg,st)) k ->
            if Cache3.alpha_contains new_arg st cache
            then continue k Hang
            else continue k @@ Later Cache3.(extend new_arg cache)
      in
      conde
        [ ((a === nil ()) &&& (b === ab))
        ; Fresh.three @@ fun h t ab' ->
            ?& [
              (a === h%t);
              (h%ab' === ab);
              (appendo t b ab');
            ]
        ]
        st

let rec reverso a b = fun st ->
  let _ = ignore @@ project2 ~msg:"Entering reverso" a b st in
  match perform (AskReversoCache (Obj.(repr a, repr b, repr 1), st)) with
  | Hang -> raise RelDivergeExn
  | Later cache ->
      let reverso a b st =
        match reverso a b st with
        | x -> x
        | effect (AskReversoCache (new_arg,st)) k ->
            if Cache3.alpha_contains new_arg st cache
            then continue k Hang
            else continue k @@ Later Cache3.(extend new_arg cache)
      in
      conde
        [ ((a === nil ()) &&& (b === nil ()))
        ; Fresh.three @@ fun h t a' ->
            (a === h%t) &&&
            (par_conj_exn
              (reverso t a')
              (appendo a' !<h b)
            )
        ]
        st

let () =
  runL (-1)  q  qh ("", (fun q   -> fun st ->
        let rel st = reverso (ilist [1;2;3;4]) q st in
        match rel st with
        | effect (AskAppendoCache (arg,_)) k ->
              (* printf "handlig askAppendo %s %d\n" __FILE__ __LINE__; *)
              continue k @@ Later Cache3.(extend arg empty)
        | effect (AskReversoCache (arg,_)) k ->
              (* printf "handlig askReverso\n"; *)
              continue k @@ Later Cache3.(extend arg empty)
        | stream -> stream
  ));
  ()
*)
