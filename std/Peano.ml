type nat = int

let wf n =
  if n < 0 then invalid_arg (Printf.sprintf "Nat.wf: negative number %d" n) else n
;;

let lift op m n = op (wf m) (wf n)
let ( + ) = lift ( + )
let ( - ) = lift ( - )
let ( * ) = lift ( * )
let ( / ) = lift ( / )
let ( mod ) = lift ( mod )
let ( < ) = lift ( < )
let ( <= ) = lift ( <= )
let ( > ) = lift ( > )
let ( >= ) = lift ( >= )
let min = lift min
let max = lift max

module HO = struct
  include OCanren
  include PeanoRaw.HO

  type nat = OCanren.Std.Nat.ground [@@deriving gt ~options:{ show; fmt; gmap }]
  type nat_logic = OCanren.Std.Nat.logic [@@deriving gt ~options:{ show; fmt; gmap }]
  type nat_injected = OCanren.Std.Nat.injected

  let nat = OCanren.Std.Nat.ground
  let nat_logic = OCanren.Std.Nat.logic
  let nat_prj_exn = OCanren.Std.Nat.prj_exn
  let nat_reify = OCanren.Std.Nat.reify
  let from_int i = Std.nat i
  let ( + ) = add
  let ( - ) = sub
  let ( * ) = mul
  let ( / ) x y z = fresh x' (x x') (mul (( === ) z) y x')

  let ( mod ) x y z =
    fresh (z' y') (( / ) x y z') (( * ) (( === ) z') y y' &&& ( - ) x (( === ) y') z)
  ;;

  let ( < ) = lt
  let ( <= ) = le
  let ( > ) = gt
  let ( >= ) = ge
end

module FO = struct
  open OCanren

  let lift op x y z = op (( === ) x) (( === ) y) z
  let ( + ) = lift HO.( + )
  let ( - ) = lift HO.( - )
  let ( * ) = lift HO.( * )
  let ( / ) = lift HO.( / )
  let ( mod ) = lift HO.( mod )
  let ( < ) = lift HO.lt
  let ( <= ) = lift HO.le
  let ( > ) = lift HO.gt
  let ( >= ) = lift HO.ge
  let min = lift HO.min
  let max = lift HO.max
end
