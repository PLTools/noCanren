open Printf
open GT
open MiniKanren
open Std
open Tester

@type token = Id | Add | Mul with show

let show_token = show(token)

module GExpr =
  struct

    module T =
      struct
        @type 'self t  = I | A of 'self * 'self | M of 'self * 'self with show, gmap

        let fmap f x = gmap(t) f x
     end

  include T
  include Fmap (T)

  type  expr = expr t
  type lexpr = lexpr t logic
  type fexpr = (expr, lexpr) injected

  let rec show_expr  e = show T.t show_expr e
  let rec show_lexpr e = show(logic) (show T.t show_lexpr) e

end

open GExpr

let i ()  : fexpr = inj @@ distrib  I
let a a b : fexpr = inj @@ distrib @@ A (a,b)
let m a b : fexpr = inj @@ distrib @@ M (a,b)

let sym t i i' = cont_delay (i === t % i')
  (* fresh (x xs)
    (i === x%xs) (t === x) (i' === xs) *)

let eof i = cont_delay (i === nil ())

let (|>) x y = fun i i'' r'' -> cont_delay @@
  Fresh.two (fun i' r' ->
      (x i  i' r') <&> (y r' i' i'' r'')
  )

let (<|>) x y = fun i i' r -> cont_delay @@
  conde [x i i' r; y i i' r]

let rec pId n n' r = cont_delay @@ (sym !!Id n n') <&> (r === i())

and pAdd i i' r = cont_delay @@ (pMulPlusAdd <|> pMul) i i' r

and pMulPlusAdd i i' r = cont_delay @@ (
      pMul |>
      (fun r i i'' r'' ->
         Fresh.two (fun r' i' ->
           (r'' === (a r r')) <&>
           (sym !!Add i i') <&>
           (pAdd i' i'' r')
      ))) i i' r

and pMul i i' r = cont_delay @@ (pIdAstMul <|> pId) i i' r

and pIdAstMul i i' r = cont_delay @@ (
      pId |>
      (fun r i i'' r'' ->
         Fresh.two (fun r' i' ->
           (r'' === (m r r')) <&>
           (sym !!Mul i i') <&>
           (pMul i' i'' r')
      ))) i i' r

and pTop i i' r = cont_delay @@ pAdd i i' r

let pExpr i r = cont_delay @@ Fresh.one (fun i' -> (pTop i i' r) <&> (eof i'))

let runE_exn n = run_exn show_expr n
let show_stream xs = show(List.ground) show_token xs

let _ =
  runE_exn   (-1)   q   qh (REPR (fun q -> pExpr (list (!!) [Id]) q                  ));
  runE_exn   (-1)   q   qh (REPR (fun q -> pExpr (list (!!) [Id; Mul; Id]) q         ));
  runE_exn   (-1)   q   qh (REPR (fun q -> pExpr (list (!!) [Id; Mul; Id; Mul; Id]) q));
  runE_exn   (-1)   q   qh (REPR (fun q -> pExpr (list (!!) [Id; Mul; Id; Add; Id]) q));
  runE_exn   (-1)   q   qh (REPR (fun q -> pExpr (list (!!) [Id; Add; Id; Mul; Id]) q));
  runE_exn   (-1)   q   qh (REPR (fun q -> pExpr (list (!!) [Id; Add; Id; Add; Id]) q));
  run_exn show_stream (-1)   q   qh (REPR (fun q -> pExpr q (m (i ()) (i ()))        ));
  ()
