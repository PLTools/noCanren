open OCanren
open Tester
open WGC.HO

let empty_side = ctor_side !!false !!false !!false !!false
let full_side = ctor_side !!true !!true !!true !!true
let init = Std.pair full_side empty_side
let final = Std.pair empty_side full_side

let _ =
  let run n =
    run_r
      (Std.List.reify move_reify)
      (GT.show Std.List.logic @@ GT.show move_logic)
      n
      q
      qh
  in
  run 1 ("First answer", fun q -> WGC.FO.eval init q final)
;;
