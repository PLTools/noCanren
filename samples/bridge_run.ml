open GT
open OCanren
open OCanren.Std
open Tester
open Bridge.HO

(*************************************************)
module Gperson = struct
  [%%distrib
  type nonrec t = gperson =
    | A
    | B
    | C
    | D
  [@@deriving gt ~options:{ show; gmap }]

  type nonrec ground = t]
end

module Gstep = struct
  [%%distrib
  type nonrec 'a0 t = 'a0 gstep =
    | One of 'a0
    | Two of 'a0 * 'a0
  [@@deriving gt ~options:{ show; gmap }]

  type nonrec ground = Gperson.ground t]
end

let show_person = function
  | A -> "A"
  | B -> "B"
  | C -> "C"
  | D -> "D"
;;

let show_step f = function
  | One x -> f x
  | Two (x, y) -> Printf.sprintf "(%s, %s)" (f x) (f y)
;;

let myshow x = show List.ground (show_step show_person) x

(*************************************************)

let rec int2nat i = if i = 0 then o () else s @@ int2nat @@ (i - 1)

(** For high order conversion **)
let getAnswer q t r = getAnswer (( === ) q) t r

let _ =
  run_r
    (Std.List.prj_exn Gstep.prj_exn)
    myshow
    1
    q
    qh
    ("answers", fun q -> getAnswer q standartTimes (int2nat 17 |> some))
;;
