open Printf
open GT
open MiniKanren
open Std
open Tester
open Stlc

module Ast =
  struct
    module T =
      struct
        @type ('a, 'b) t =
        | MetaVar of int
        | P   of 'a      (* primitive *)
        | Arr of 'b * 'b (* arrow *)
        with gmap, show

       let fmap f g subj = gmap(t) f g subj
       (*let fmap f g subj =
         printf "fmap `%s`\n%!" (generic_show subj);
         match subj with
         | MetaVar n -> MetaVar n
         | P a -> P (f a)
         | Arr (a,b) -> Arr (g a, g b)*)

       let shallower n =  MetaVar (Obj.magic n)
       let make_var = Some shallower
     end

  include T
  module F = Fmap2(T)
  include F

  type rast = (string,       rast) t
  type last = (string logic, last) t logic
  type ftyp = (rast, last) injected

  let p s     : ftyp = inj @@ distrib @@ P s
  let arr x y : ftyp = inj @@ distrib @@ Arr (x,y)


  let rec show_rast typ = show(t) (show string) show_rast typ
  let rec show_last typ = show(logic) (show(t) (show(logic) @@ show string) show_last) typ

  let plain_reify c ast =
    let rec ans c x =
      F.plain_reify MiniKanren.plain_reify ans c x
    in
    ans c ast
end

open Ast

let demo1 q =
  Fresh.two (fun u v ->
    q === (arr (arr (p !!"asdf") u) (arr (p !!"asdf") v))
  )

let runA n = run_exn2 Ast.plain_reify Ast.show_rast n

let () =
  runA     1   q   qh (REPR (fun q -> demo1 q                                       ))
