open MiniKanren
open Tester
open Printf
open GT

let rec lto x y =
  conde
  [ Fresh.one (fun y' ->
      (x === Nat.zero) &&&
      (y === Nat.succ y'))
  ; Fresh.two (fun x2 y2 ->
      (x === Nat.succ x2) &&&
      (y === Nat.succ y2) &&&
      (lto x2 y2)
    )
  ]

let geo x y = conde [ x === y; lto y x ]

let rec minimumo xs m =
  conde
  [ (xs === !< m)
  ; Fresh.three (fun x t y ->
      (xs === x % t) &&&
      (minimumo t y) &&&
      (conde
        [ lto x y &&& (x === m)
        ; geo x y &&& (y === m)
        ])
    )
  ]

let minmaxo a b min max =
  conde
    [ (min === a) &&& (max === b) &&& (lto a b)
    ; (max === a) &&& (min === b) &&& (geo a b)
    ]

let rec smallesto l s l' = conde
  [ (l === !< s) &&& (l' === nil())
  ; Fresh.five (fun h t s' t' max ->
      (l' === max % t') &&&
      (l === h % t) &&&
      (minmaxo h s' s max) &&&
      (smallesto t s' t')
    )
  ]

type ask_result = Hang | Later of Cache3.t;;
effect AskSortoCache : (Obj.t * Obj.t * Obj.t) * State.t -> ask_result;;
let rec sorto xs ys = fun st ->
  let cache =
    let arg = Obj.(repr xs, repr ys, repr 1) in
    try
      match perform (AskSortoCache (arg, st)) with
      | Hang -> raise (RelDivergeExn (sorto xs ys, st))
      | Later cache -> cache
    with Unhandled -> Cache3.(extend arg empty)
  in
  let sorto a b = fun st ->
    match sorto a b st with
    | exception (RelDivergeExn _ as e) -> raise e
    | ss -> ss
    | effect (AskSortoCache (new_arg, new_st)) k ->
        if Cache3.alpha_contains new_arg new_st cache
        then continue k Hang
        else continue k @@ Later Cache3.(extend new_arg cache)
  in
  conde
    [ (xs === nil ()) &&& (ys === nil ())
    ; Fresh.three @@ fun s xs' ys' ->
        (ys === s % ys') &&&
        (* (sorto xs' ys') &&&
        (smallesto xs s xs') *)

        (par_conj_exn
          (sorto xs' ys')
          (smallesto xs s xs')
        )
    ]
    st


(* Making regular sorting from relational one *)
let sort l =
  List.to_list Nat.to_int @@
  run q (sorto @@ inj_nat_list l)
        (fun qs -> Stream.hd qs |> (fun rr -> rr#prj))

(* Veeeeery straightforward implementation of factorial *)
let rec fact = function 0 -> 1 | n -> n * fact (n-1)


(* Making permutations from relational sorting *)
let perm l =
  List.map (List.to_list Nat.to_int) @@
  run q (fun q -> sorto q @@ inj_nat_list (List.sort Pervasives.compare l))
        (fun qs ->
          qs |> Stream.take ~n:(fact @@ List.length l) |>
          List.map (fun rr -> rr#prj))

let () =
  TimeHelper.wrap (fun () -> printf "%s\n\n%!" (show(list) (show(int)) @@ sort [6; 5; 4; 3; 2; 1]));
  TimeHelper.wrap (fun () -> printf "%s\n\n%!" (show(list) (show(list) (show(int))) @@ perm [1; 2; 3; 4; 5]));
  ()
