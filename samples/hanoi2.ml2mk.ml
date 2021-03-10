type nat    = Z | S of nat
type set    = Set of nat list * nat list * nat list
type pin    = A | B | C

let rec less x y =
  match y with
  | S y' -> match x with
           | Z    -> true
           | S x' -> less x' y'


let extra pin =
  match pin with
  | (A, B) -> C
  | (B, A) -> C
  | (A, C) -> B
  | (C, A) -> B
  | (B, C) -> A
  | (C, B) -> A

let select s pin =
  match s with
  | Set (x, y, z) ->
    match pin with
    | A -> x
    | B -> y
    | C -> z

let permut move s =
  match move with
  | (x, y) -> Set (select s x, select s y, select s (extra move))

let tumrep move s =
  match s with
  | Set (x, y, z) ->
    match move with
    | (A, B) -> Set (x, y, z)
    | (B, A) -> Set (y, x, z)
    | (A, C) -> Set (x, z, y)
    | (C, A) -> Set (y, z, x)
    | (B, C) -> Set (z, x, y)
    | (C, B) -> Set (z, y, x)

let[@tabled] rec eval p s =
  match p with
  | []         -> s
  | move :: p' ->
    match move with
    | (x, y) ->
      eval p' (
        if x = y
          then s
          else
            match permut move s with
            | Set (onA, onB, onC) ->
              tumrep move (
                match onA with
                | topA :: restA ->
                  match onB with
                  | []                           -> Set (restA, [topA], onC)
                  | topB :: _  when topB >= topA ->
                    match less topA topB with
                    | true -> Set (restA, topA :: onB, onC)))
