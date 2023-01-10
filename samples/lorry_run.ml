open GT
open OCanren
open OCanren.Std
open Tester
open Lorry.HO

(*************************************************)

module Gnat = struct
  [%%distrib
  type nonrec 'a t = 'a gnat =
    | O
    | S of 'a
  [@@deriving gt ~options:{ show; gmap }]

  type ground = ground t]
end

module Gstep = struct
  [%%distrib
  type nonrec 'a0 t = 'a0 gstep =
    | Left of 'a0
    | Right of 'a0
    | Fill
    | Pour of 'a0
  [@@deriving gt ~options:{ show; gmap }]

  type nonrec ground = Gnat.ground t]
end

let show_number num =
  let rec helper = function
    | O -> 0
    | S x -> 1 + helper x
  in
  string_of_int @@ helper num
;;

let show_step = function
  | Left x -> Printf.sprintf "L%s" @@ show_number x
  | Right x -> Printf.sprintf "R%s" @@ show_number x
  | Fill -> "F"
  | Pour x -> Printf.sprintf "P%s" @@ show_number x
;;

let myshow x = show List.ground show_step x

(*************************************************)

let rec of_int i = if i = 0 then o () else s @@ of_int @@ (i - 1)

(** For high order conversion **)
let checkAnswer a q p r = checkAnswer (( === ) a) (( === ) q) (( === ) p) r

let () =
  run_r
    (List.prj_exn Gstep.prj_exn)
    myshow
    1
    q
    qh
    ("answers", fun q -> checkAnswer q (of_int 8) (of_int 5) (some @@ of_int 22))
;;
