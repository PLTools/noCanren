(*
  Quines stuff by Dmitrii Rozplokhas. Adopted from
  https://raw.githubusercontent.com/rozplokhas/OCanren/master/regression/test015.ml
*)

open Printf
open GT
open MiniKanren

let list_combine3 xs ys zs =
  let rec helper acc = function
    | (x::xs, y::ys, z::zs) -> helper ((x,y,z)::acc) (xs,ys,zs)
    | ([],[],[]) -> List.rev acc
    | _ -> failwith "bad argument of list_combine3"
  in
  helper [] (xs,ys,zs)

let list_iter3 f xs ys zs =
  let rec helper = function
    | (x::xs, y::ys, z::zs) -> f (x,y,z); helper (xs,ys,zs)
    | ([],[],[]) -> ()
    | _ -> failwith "bad argument of list_combine3"
  in
  helper (xs,ys,zs)

module Gterm = struct
  module X = struct
    type ('s, 'xs) t =
      | Symb  of 's
      | Seq   of 'xs [@@deriving gt {show}, showT]

    let fmap f g = function
    | Symb s -> Symb (f s)
    | Seq xs -> Seq (g xs)

    let t = {t with
      gcata = ();
      plugins = object
        method gmap = fmap (* t.plugins#gmap *)
        method show fa fb bx =
           GT.transform(t)
              (GT.lift fa) (GT.lift fb)
              (object inherit ['a,'b] show_t_t
                method c_Symb _ s str =
                  sprintf "(symb '%s)" (str.GT.fx ())
                method c_Seq  _ _ xs =
                  sprintf "(seq %s)" (xs.GT.fx ())
               end)
              ()
              bx
       end
    }

  end
  include X
  include Fmap2(X)

  type rterm = (string, rterm List.ground) X.t
  type lterm = (string logic, lterm List.logic) X.t logic
  type fterm = (rterm, lterm) injected

  let rec show_rterm : rterm -> string = fun t -> GT.(show X.t (fun s -> s) (show List.ground show_rterm)) t
  let rec show_lterm : lterm -> string =
    fun x -> GT.(show logic @@ show X.t (show logic (fun s -> s)) (show List.logic show_lterm) ) x

  let rec to_logic : rterm -> lterm = fun term ->
    Value (GT.(gmap X.t) (fun s -> Value s) (List.to_logic to_logic) term)

  let symb s : fterm = inj @@ distrib @@ Symb s
  let seq xs : fterm = inj @@ distrib @@ Seq xs
end

let rec gterm_reifier c : Gterm.fterm -> Gterm.lterm =
  Gterm.reify ManualReifiers.string (List.reify gterm_reifier) c

module Gresult = struct
  module X = struct
    type ('s, 't, 'xs) t =
    | Closure of 's * 't * 'xs
    | Val     of 't
    [@@deriving gt {show}]

    let fmap f g h = function
    | Closure (a,b,c) -> Closure (f a, g b, h c)
    | Val b -> Val (g b)

    let t = {
      gcata = ();
      plugins = object
        method gmap = fmap
        method show = t.plugins#show
      end
    }
  end

  include Fmap3(X)
  type rresult = (string, Gterm.rterm, (string * rresult) List.ground) X.t
  type lresult = (string logic, Gterm.lterm, (string logic * lresult) logic List.logic) X.t logic
  type fresult = (rresult, lresult) injected

  let closure s t xs = inj @@ distrib @@ X.Closure (s,t,xs)
  let val_ t         = inj @@ distrib @@ X.Val t

  let show_string = GT.(show string)
  let show_stringl = GT.(show logic) show_string

  let rec show_rresult r = GT.(show X.t show_string Gterm.show_rterm
      @@ show List.ground (show pair show_string show_rresult)) r
  let rec show_lresult r = GT.(show logic @@ show X.t show_stringl Gterm.show_lterm
    @@ show List.logic (show logic @@ show pair show_stringl show_lresult)) r

  let rec to_logic : rresult -> lresult = fun res ->
    let arg3 xs = List.to_logic (fun (a,b) -> Value (Value a, to_logic b)) xs in
    Value (GT.(gmap X.t) (fun s -> Value s) (Gterm.to_logic) arg3 res)

end


let rec gresult_reifier c : Gresult.fresult -> Gresult.lresult =
  let open ManualReifiers in
  Gresult.reify string gterm_reifier
    (List.reify (pair string gresult_reifier))
    c

let (!!) x = inj @@ lift x

open Gterm
open Gresult

type fenv = ( (string * rresult) List.ground,
              (string logic * lresult) logic List.logic) injected

let show_reif_env h e =
  GT.(show List.logic @@ show logic @@
        show pair  (show logic (fun s -> s)) show_lresult) @@
  (List.reify ManualReifiers.(pair string gresult_reifier))
  h e

let (===!) = (===)
let (===!!) = (===)

let rec lookupo x env t =
  fresh (rest y v)
    ((inj_pair y v) % rest === env)
    (conde [
        (y ===! x) &&& (v ===!! t);
        (y =/= x) &&& (lookupo x rest t)
      ])

let rec not_in_envo x env =
  conde
    [ fresh (y v rest)
        (env === (inj_pair y v) % rest)
        (y =/= x)
        (not_in_envo x rest)
    ; (nil () === env)
    ]

let rec proper_listo es env rs =
  conde
    [ ((nil ()) === es) &&& ((nil ()) === rs)
    ; fresh (e d te td)
        (es === e  % d)
        (rs === te % td)
        (evalo e env (val_ te))
        (proper_listo d env td)
    ]

and evalo (term: fterm) (env: fenv) (r: fresult) =
  conde
  [ fresh (t)
    (term === seq ((symb !!"quote") %< t))
    (r ===! (val_ t))
    (not_in_envo !!"quote" env)
  ; fresh (es rs)
      (term === seq ((symb !!"list") % es) )
      (r ===! val_ (seq rs))
      (not_in_envo !!"list" env)
      (proper_listo es env rs)
  ; fresh (s)
      (term === (symb s))
      (lookupo s env r)
  ; fresh (func arge arg x body env')
      (term === seq (func %< arge))
      (evalo arge env arg)
      (evalo func env (closure x body env') )
      (evalo body ((inj_pair x arg) % env') r)

  ; fresh (x body)
      (term === seq ( (symb !!"lambda") %
                      (seq (!< (symb x)) %< body)
                    ) )
      (not_in_envo !!"lambda" env)
      (r ===! (closure x body env))
  ]

let ( ~~ ) s  = symb @@ inj @@ lift s
let s      tl = seq (inj_listi tl)

let nil = nil ()

let quineso q = (evalo q nil (val_ q))

let twineso q p =
  (q =/= p) &&& (evalo q nil (val_ p)) &&& (evalo p nil (val_ q))

let thrineso x =
  (* let (=//=) = diseqtrace @@ show_reif_term in *)
  fresh (p q r)
    (p =/= q)
    (q =/= r)
    (r =/= p)
    (evalo p nil (val_ q))
    (evalo q nil (val_ r))
    (evalo r nil (val_ p))
    ((inj_triple p q r) === x)

let wrap_term rr = rr#refine gterm_reifier ~inj:Gterm.to_logic |> show_lterm
let wrap_result rr = rr#refine gresult_reifier ~inj:Gresult.to_logic |> show_lresult

let find_quines ~verbose n = run q quineso @@ fun qs ->
  Stream.take ~n qs |> List.iter (fun q ->
    if verbose
    then printf "%s\n\n" (wrap_term q)
    else ()
  )

let find_twines ~verbose n =
  run qr (fun q r -> twineso q r)
    (fun qs rs ->
      let s1 = Stream.take ~n qs in
      let s2 = Stream.take ~n rs in
      List.iter2 (fun q r ->
        if verbose
        then printf "%s,\n%s\n\n" (wrap_term q) (wrap_term r)
        else ()
      ) s1 s2
    )

let wrap3terms t =
  t#refine
    (ManualReifiers.triple gterm_reifier gterm_reifier gterm_reifier)
    ~inj:(fun (a,b,c) ->
        Value (Gterm.to_logic a,Gterm.to_logic b,Gterm.to_logic a) )
  |> (function
      | Var _ -> assert false
      | Value (a,b,c) ->
          printfn "* %s\n  %s\n  %s\n"
          (Gterm.show_lterm a)
          (Gterm.show_lterm b)
          (Gterm.show_lterm c)
      )

let find_thrines ~verbose n =
  run q thrineso @@ fun xs ->
      Stream.take ~n xs |>
      List.iter (fun t ->
          if verbose then
            let () = wrap3terms t in
            print_newline ()
          else ()
        )

let () = find_thrines ~verbose:true 1
