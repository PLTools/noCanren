open MiniKanren
open Tester
open Printf
open GT

let show_int       = show(int)
let show_int_list  = (show(List.ground) (show int))
let show_intl_list = (show(List.logic ) (show(logic) (show int)))
let runL n         = runR (List.reify ManualReifiers.int) show_int_list show_intl_list n

let runN n         = runR Nat.reify GT.(show Nat.ground) GT.(show Nat.logic) n

let ilist xs = inj_list (!!) xs
let just_a a = a === !!5
let show_reify_list h t = show_intl_list @@ List.reify ManualReifiers.int h t
let pp_nat_logic n =
  (* printfn "Got %s" (GT.show Nat.logic n); *)
  let rec helper acc = function
  | (Var _) as v when acc = 0 -> GT.show Nat.logic v
  | (Var _) as v -> sprintf "%d + %s" acc (GT.show Nat.logic v)
  | Value O    -> string_of_int acc
  | Value (S x) -> helper (acc+1) x
  in
  helper 0 n

let show_reify_nat  h t = pp_nat_logic @@ Nat.reify h t
let project3 ~msg x y z = project3 ~msg show_reify_nat x y z
let project2 ~msg x y   = project2 ~msg show_reify_nat x y
let show_reify_key h (x,y,z) =
  sprintf "(%s, %s, %s)"
    (show_reify_list h @@ Obj.magic x)
    (show_reify_list h @@ Obj.magic y)
    (show_reify_list h @@ Obj.magic z)

type ask_result = Hang | Later of Cache3.t;;

(* effect AskAppendoCache : (Obj.t * Obj.t * Obj.t) * State.t -> ask_result;;
effect AskReversoCache : (Obj.t * Obj.t * Obj.t) * State.t -> ask_result;;

let rec appendo a b ab = fun st ->
  let _ = ignore @@ project3 ~msg:"Entering appendo" a b ab st in

  let cache =
    let arg = Obj.(repr a, repr b, repr ab) in
    try
      match perform (AskAppendoCache (arg, st)) with
      | Hang -> raise RelDivergeExn
      | Later cache -> cache
    with Unhandled -> Cache3.(extend arg empty)
  in

  let appendo a b ab = fun st ->
    match appendo a b ab st with
    | ss -> ss
    | effect (AskAppendoCache (new_arg, st)) k ->
        if Cache3.alpha_contains new_arg st cache
        then let () = print_endline "Hand" in continue k Hang
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
    st *)

effect AskMuloCache : (Obj.t * Obj.t * Obj.t) * State.t -> ask_result;;
let rec mulo x y z st =
  let () = ignore @@ project3 ~msg:"mulo" x y z st in
  let cache =
    let arg = Obj.(repr x, repr y, repr z) in
    try
      match perform (AskMuloCache (arg, st)) with
      | Hang -> raise RelDivergeExn
      | Later cache -> cache
    with Unhandled -> Cache3.(extend arg empty)
  in

  let mulo a b ab = fun st ->
    match mulo a b ab st with
    | ss -> ss
    | effect (AskMuloCache (new_arg, st)) k ->
        if Cache3.alpha_contains new_arg st cache
        then let () = print_endline "Hang" in continue k Hang
        else continue k @@ Later Cache3.(extend new_arg cache)
  in
  conde
  [ (x === Nat.zero) &&& (x === z)
  ; Fresh.two @@ fun xt zt ->
      (x === Nat.succ xt) &&&
      (par_conj_exn
        (Nat.addo zt y z)
        (mulo xt y zt)
      )
  ] st

let () =
  runN (-1)  q  qh ("", (fun q   -> mulo q Nat.(inj@@of_int 2) Nat.(inj@@of_int 6)     ));
  runN (-1)  q  qh ("", (fun q   -> mulo q Nat.(inj@@of_int 2) Nat.(inj@@of_int 5)     ));
  (* should diverge after 1st answer *)
  runN (-1)  q  qh ("", (fun q   -> mulo Nat.(inj@@of_int 3) Nat.(inj@@of_int 2) q     ));
  ()
