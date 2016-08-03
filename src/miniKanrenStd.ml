open ImplicitPrinters

module type MINIKANREN_CORE = sig
  type state
  type goal
  type 'a logic

  val call_fresh : ('a logic -> goal) -> goal
  val call_fresh_named : string -> ('a logic -> goal) -> goal

  val inj: {S : ImplicitPrinters.SHOW} -> S.t -> S.t logic

  val (===): 'a logic -> 'a logic -> goal
  val (&&&): goal -> goal -> goal
  val conde: goal list -> goal

  type 'a llist
  val (%) : 'a logic -> 'a llist logic -> 'a llist logic
  val (%<) : 'a logic -> 'a logic -> 'a llist logic
  val (!<) : 'a logic -> 'a llist logic
  val llist_nil: 'a llist logic

  implicit module Show_logic {X: SHOW} : (SHOW with type t = X.t logic)
  implicit module Show_llist {X: SHOW} : (SHOW with type t = X.t llist)
end

let fst3 (x,_,_) = x

module Make(MK: MINIKANREN_CORE) = struct
  open MK

  let (!) = inj

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

  let rec combine : {X:SHOW} -> {Y:SHOW} -> X.t llist logic -> Y.t llist logic ->
                    (X.t logic * Y.t logic) llist logic -> goal
    = fun {X:SHOW} {Y:SHOW} (xs: X.t llist logic) (ys: Y.t llist logic)
          (ans: (X.t logic * Y.t logic) llist logic) ->
    call_fresh @@ fun (temp: (X.t logic * Y.t logic) llist logic) ->
    fresh (hx tx hy ty )
          (list_cons xs hx tx)
          (list_cons ys hy ty)
          (combine {X} {Y} tx ty temp)
          (ans === (inj (hx,hy)) % temp)

end
