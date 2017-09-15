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
    (show_reify_nat h @@ Obj.magic x)
    (show_reify_nat h @@ Obj.magic y)
    (show_reify_nat h @@ Obj.magic z)

type ask_result = Hang | Later of Cache3.t;;

effect AskAddoCache : (Obj.t * Obj.t * Obj.t) * State.t -> ask_result;;
let rec addo x y z = fun st ->
  let () = ignore @@ project3 ~msg:"addo" x y z st in

  (* let shit = ref (fun _ _ _ -> let () = printfn "XXXXXXX" in success) in *)
  let cache =
    let arg = Obj.(repr x, repr y, repr z) in
    try
      match perform (AskAddoCache (arg, st)) with
      | Hang -> raise (RelDivergeExn (addo x y z, st))
      | Later cache ->
          printfn "Got an `addo` cache of size %d" (Cache3.size cache);
          cache
    with Unhandled -> Cache3.(extend arg empty)
  in
  let addo a b ab = fun st ->
    match addo a b ab st with
    | exception (RelDivergeExn _ as e) ->
      printf "FUCK %s %d\n%!" __FILE__ __LINE__;
      raise e
    | ss -> ss
    | effect (AskAddoCache (new_arg, new_st)) k ->
        (* let () =
          printfn "got request for cache for";
          ignore @@ project3 ~msg:"  old call addo" x y z st
        in *)
        if Cache3.alpha_contains ~printer:show_reify_key new_arg new_st cache
        then continue k Hang
        else continue k @@ Later Cache3.(extend new_arg cache)
  in

  (* shit := addo; *)

  conde
    [ (x === Nat.zero) &&& (z === y)
    ; Fresh.two @@ fun x' z' ->
        (x === Nat.succ x') &&&
        (z === Nat.succ z') &&&
        (addo x' y z')
    ]
    st

effect AskMuloCache : (Obj.t * Obj.t * Obj.t) * State.t -> ask_result;;
let rec mulo x y z st =
  let () = ignore @@ project3 ~msg:"mulo" x y z st in

  let shit = ref (fun _ _ _ -> success) in

  let cache =
    let arg = Obj.(repr x, repr y, repr z) in
    try
      match perform (AskMuloCache (arg, st)) with
      | Hang -> raise (RelDivergeExn (!shit x y z, st))
      | Later cache -> printfn "Got a cache of size %d" (Cache3.size cache); cache
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

  shit := mulo;
  conde
  [ (x === Nat.zero) &&& (x === z)
  ; Fresh.two @@ fun xt zt ->
      (x === Nat.succ xt) &&&
      (par_conj_exn
        (addo zt y z)
        (mulo xt y zt)
      )
  ] st

let () =
  runN (-1)  q  qh ("mulo q 2 6", (fun q   -> mulo q Nat.(inj@@of_int 2) Nat.(inj@@of_int 6)     ));
  (* runN (-1)  q  qh ("mulo q 2 5", (fun q   -> mulo q Nat.(inj@@of_int 2) Nat.(inj@@of_int 5)     )); *)
  (* should diverge after 1st answer *)
  (* runN (-1)  q  qh ("mulo 3 2 ?", (fun q   -> mulo Nat.(inj@@of_int 3) Nat.(inj@@of_int 2) q     )); *)
  (* runN (-1)  q  qh ("mulo 2 1 ?", (fun q   -> mulo Nat.(inj@@of_int 2) Nat.(inj@@of_int 1) q     )); *)
  (* runN (-1)  q  qh ("mulo 2 0 ?", (fun q   -> mulo Nat.(inj@@of_int 2) Nat.(inj@@of_int 0) q     )); *)
  (* runN (-1)  q  qh ("mulo 1 1 (s ?)", (fun q  -> mulo Nat.(inj@@of_int 1) Nat.(inj@@of_int 1) (Nat.succ q)     )); *)
  (* runN (-1)  q  qh ("mulo 1 2 ?", (fun q  -> mulo Nat.(inj@@of_int 1) Nat.(inj@@of_int 2) q     )); *)
  (* runN (-1)  q  qh ("mulo 2 2 ?", (fun q  -> mulo Nat.(inj@@of_int 2) Nat.(inj@@of_int 2) q     )); *)

  ()
