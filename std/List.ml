let length = List.length
let append = List.append
let mem = List.mem
let rev = List.rev
let lookup = List.assoc_opt
let assoc = List.assoc
let map = List.map
let mapi = List.mapi
let map2 = List.map2
let filter = List.filter
let fold_left = List.fold_left
let fold_left2 = List.fold_left2
let fold_right = List.fold_right
let fold_right2 = List.fold_right2
let exists = List.exists
let for_all = List.for_all
let find_opt = List.find_opt
let nth = List.nth
let nth_opt = List.nth_opt

module HO = struct
  type 'a list = 'a OCanren.Std.List.ground [@@deriving gt ~options:{ show; fmt; gmap }]

  type 'a list_logic = 'a OCanren.Std.List.logic
  [@@deriving gt ~options:{ show; fmt; gmap }]

  type 'a list_injected = 'a OCanren.Std.List.injected

  let list = OCanren.Std.List.ground
  let list_logic = OCanren.Std.List.logic
  let list_prj_exn = OCanren.Std.List.prj_exn
  let list_reify = OCanren.Std.List.reify

  open ListRaw.HO

  let length = length
  let append = append
  let mem = mem
  let rev = rev
  let lookup = lookup
  let assoc = assoc
  let map = map
  let map2 = map2
  let mapi = mapi
  let filter = filter
  let fold_left = fold_left
  let fold_left2 = fold_left2
  let fold_right = fold_right
  let fold_right2 = fold_right2
  let exists = exists
  let for_all = for_all
  let find_opt = find_opt
  let nth = nth
  let nth_opt = nth_opt
end

module FO = struct
  open OCanren

  let lift1 op x y = op (( === ) x) y
  let lift2 op x y z = op (( === ) x) (( === ) y) z
  let lift_ho2_2 op f x y = op (fun x y -> fresh x0 (x x0) (f x0 y)) (( === ) x) y

  let lift_ho3_2 op f x y =
    op (fun x y z -> fresh (x0 y0) (x x0) (y y0) (f x0 y0 z)) (( === ) x) y
  ;;

  let lift_ho3_3 op f x y z =
    op (fun x y z -> fresh (x0 y0) (x x0) (y y0) (f x0 y0 z)) (( === ) x) (( === ) y) z
  ;;

  let lift_ho4_4 op f x y z v =
    op
      (fun x y z v -> fresh (x0 y0 z0) (x x0) (y y0) (z z0) (f x0 y0 z0 v))
      (( === ) x)
      (( === ) y)
      (( === ) z)
      v
  ;;

  let length x = lift1 HO.length x
  let append x = lift2 HO.append x
  let mem x = lift2 HO.mem x
  let rev x = lift1 HO.rev x
  let lookup x = lift2 HO.lookup x
  let assoc x = lift2 HO.assoc x
  let map x = lift_ho2_2 HO.map x
  let mapi x = lift_ho3_2 HO.mapi x
  let map2 x = lift_ho3_3 HO.map2 x
  let filter x = lift_ho2_2 HO.filter x
  let fold_left x = lift_ho3_3 HO.fold_left x
  let fold_left2 x = lift_ho4_4 HO.fold_left2 x
  let fold_right x = lift_ho3_3 HO.fold_right x
  let fold_right2 x = lift_ho4_4 HO.fold_right2 x
  let exists x = lift_ho2_2 HO.exists x
  let for_all x = lift_ho2_2 HO.for_all x
  let find_opt x = lift_ho2_2 HO.find_opt x
  let nth x = lift2 HO.nth x
  let nth_opt x = lift2 HO.nth_opt x
end
