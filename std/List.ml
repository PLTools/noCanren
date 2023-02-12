let length = List.length
let append = List.append
let member = List.mem
let rev = List.rev
let lookup = List.assoc_opt
let assoc = List.assoc
let map = List.map
let filter = List.filter
let fold_left = List.fold_left
let fold_right = List.fold_right
let any = List.exists
let all = List.for_all

module HO = struct
  type 'a list = 'a OCanren.Std.List.ground
  type 'a list_logic = 'a OCanren.Std.List.logic
  type 'a list_injected = 'a OCanren.Std.List.injected

  let list = OCanren.Std.List.ground
  let list_logic = OCanren.Std.List.logic
  let list_prj_exn = OCanren.Std.List.prj_exn
  let list_reify = OCanren.Std.List.reify

  open ListRaw.HO

  let length = length
  let append = append
  let member = member
  let rev = rev
  let lookup = lookup
  let assoc = assoc
  let map = map
  let filter = filter
  let fold_left = fold_left
  let fold_right = fold_right
  let any = any
  let all = all
end

module FO = struct
  open OCanren

  let lift1 op x y = op (( === ) x) y
  let lift2 op x y z = op (( === ) x) (( === ) y) z
  let lift_ho_1 op f x y = op (fun x y -> fresh x0 (x x0) (f x0 y)) (( === ) x) y

  let lift_ho_2 op f x y z =
    op (fun x y z -> fresh (x0 y0) (x x0) (y y0) (f x0 y0 z)) (( === ) x) (( === ) y) z
  ;;

  let length x = lift1 HO.length x
  let append x = lift2 HO.append x
  let member x = lift2 HO.member x
  let rev x = lift1 HO.rev x
  let lookup x = lift2 HO.lookup x
  let assoc x = lift2 HO.assoc x
  let map x = lift_ho_1 HO.map x
  let filter x = lift_ho_1 HO.filter x
  let fold_left x = lift_ho_2 HO.fold_left x
  let fold_right x = lift_ho_2 HO.fold_right x
  let any x = lift_ho_1 HO.any x
  let all x = lift_ho_1 HO.all x
end
