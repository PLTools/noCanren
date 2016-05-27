module Option = struct
  type 'a t = 'a option
  let (>>=) x f = match x with Some x -> f x | None -> None
  let return x = Some x

  let iter ~f = function Some x -> f x; () | None -> ()
  let map ~f ~default = function Some x -> f x | None -> default
end

module List = struct
  include List

  let all_same ~f = function
    |[] -> true
    | x::xs ->
       let first = f x in
       List.for_all (fun y -> f y = first) xs

  let take ~n xs = ExtList.List.take n xs
  let split_nth ~n xs = ExtList.List.split_nth n xs
  let skip ~n = ExtList.List.drop n
  let fold_left ~f ~init xs = List.fold_left f init xs
  let hd_exn = List.hd
  let init = ExtList.List.init
  let map = ListLabels.map
end

let fst3 (x,_,_) = x


open Tester.M
open MiniKanren

let list_cons xs h tl = xs === h%tl
let list_hd xs h = fresh tl (xs === h%tl)
let list_tail what ans = fresh (h) (what === h % ans)

let rec list_snoc x xs ans =
  conde
    [ (xs === llist_nil) &&&  (ans === !< x)
    ; fresh (h tl ans2)
        (xs === h % tl)
        (list_snoc x tl ans2)
        (ans === x % ans2)
    ]

let rec appendo a b ab =
  conde
    [ (a === llist_nil) &&& (b === ab)
    ; fresh (h tl ab')
        (list_cons a h tl)
        (list_cons ab h ab')
        (appendo tl b ab')
    ]


let rec foldo f acc xs ans =
  conde
    [ (xs === llist_nil) &&& (acc === ans)
    ; fresh (h tl a')
        (xs === h % tl)
        (foldo f acc tl a')
        (f a' h ans)
    ]

let rec combine xs ys ans =
  fresh (hx tx hy ty temp)
        (list_cons xs hx tx)
        (list_cons ys hy ty)
        (combine tx ty temp)
        (ans === !(hx,hy) % temp)
