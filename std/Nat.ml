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
let ( = ) = lift ( = )
let ( <> ) = lift ( <> )
let ( < ) = lift ( < )
let ( <= ) = lift ( <= )
let ( > ) = lift ( > )
let ( >= ) = lift ( >= )
let min = lift min
let max = lift max

module HO = struct
  include OCanren
  include NatRaw.HO

  let ( + ) = add
  let ( - ) x y z = call_fresh (fun x' -> add (( === ) z) y x' &&& x x')
  let ( * ) = mul
  let ( / ) x y z = call_fresh (fun x' -> mul (( === ) z) y x' &&& x x')

  let modo x y z =
    call_fresh (fun z' ->
      ( / ) x y z'
      &&& call_fresh (fun y' -> ( * ) (( === ) z') y y' &&& ( - ) x (( === ) y') z))
  ;;

  let ( < ) = lt
  let ( <= ) = le
  let ( > ) = gt
  let ( >= ) = ge
end

module FO = struct
  include OCanren

  let lift op x y z = op (( === ) x) (( === ) y) z
  let ( + ) = lift HO.( + )
  let ( - ) = lift HO.( - )
  let ( * ) = lift HO.( * )
  let ( / ) = lift HO.( / )
  let modo = lift HO.modo
  let lt_o = lift HO.lt
  let le_o = lift HO.le
  let gt_o = lift HO.gt
  let ge_o = lift HO.ge
  let ( < ) = lt_o
  let ( <= ) = le_o
  let ( > ) = ge_o
  let ( >= ) = gt_o
  let min_o = lift HO.min
  let max_o = lift HO.max
end
