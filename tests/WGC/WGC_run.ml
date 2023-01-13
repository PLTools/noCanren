open OCanren
open Tester
open WGC

module Gmove = struct
  [%%distrib
  type nonrec t = HO.gmove =
    | Empty
    | Goat
    | Wolf
    | Cabbage
  [@@deriving gt ~options:{ gmap; show }]

  type ground = t]
end

let empty_side = HO.ctor_gside !!false !!false !!false !!false
let full_side = HO.ctor_gside !!true !!true !!true !!true
let init = Std.pair full_side empty_side
let final = Std.pair empty_side full_side

let _ =
  let run n =
    run_r
      (Std.List.reify Gmove.reify)
      (GT.show Std.List.logic @@ GT.show Gmove.logic)
      n
      q
      qh
  in
  run 1 ("First answer", fun q -> FO.eval init q final)
;;
