open Printf
open MiniKanren
open ImplicitPrinters

type token = Id | Add | Mul
module Show_token_explicit: (SHOW with type t = token) = struct
  type t = token
  let show = function
    | Id -> "Id"
    | Add -> "Add"
    | Mul -> "Mul"
end
implicit module Show_token = Show_token_explicit

type expr  = I | A of expr logic * expr logic | M of expr logic * expr logic
module rec Show_expr_explicit: (SHOW with type t = expr) = struct
  type t = expr
  let show = function
    | I -> "I"
    | A (l,r) ->
       sprintf "A (%s, %s)" (Show_expr_logic.show l) (Show_expr_logic.show r)
    | M (l,r) ->
       sprintf "M (%s, %s)" (Show_expr_logic.show l) (Show_expr_logic.show r)
end
and Show_expr_logic: (SHOW with type t = expr logic) = Show_logic_explicit(Show_expr_explicit)
implicit module Show_expr = Show_expr_explicit

open Tester.M

let (!) = embed

let sym t i i' =
  fresh (x xs)
    (i === x%xs) (t === x) (i' === xs)

let eof i = i === !(Nil : token llist)

let (|>) x y = fun i i'' r'' ->
  fresh (i' r')
    (x i  i' r')
    (y r' i' i'' r'')

let (<|>) x y = fun i i' r ->
  conde [x i i' r; y i i' r]

let rec pId i i' r = (sym !Id i i') &&& (r === !I)
and pAdd i i' r = (pMulPlusAdd <|> pMul) i i' r
and pMulPlusAdd i i' r = (
      pMul |>
      (fun r i i'' r'' ->
         fresh (r' i')
           (sym !Add i i')
           (r'' === !(A (r, r')))
           (pAdd i' i'' r')
      )) i i' r
and pMul i i' r = (pIdAstMul <|> pId) i i' r
and pIdAstMul i i' r= (
      pId |>
      (fun r i i'' r'' ->
         fresh (r' i')
           (sym !Mul i i')
           (r'' === !(M (r, r')))
           (pMul i' i'' r')
      )) i i' r
and pTop i i' r = pAdd i i' r

let pExpr i r = fresh (i') (pTop i i' r) (eof i')

open Tester

let _ =
  run1 ~n:1 (REPR(pExpr (of_list [Id])                   ) );
  run1 ~n:1 (REPR(pExpr (of_list [Id; Mul; Id])          ) );
  run1 ~n:1 (REPR(pExpr (of_list [Id; Mul; Id; Mul; Id]) ) );
  run1 ~n:1 (REPR(pExpr (of_list [Id; Mul; Id; Add; Id]) ) );
  run1 ~n:1 (REPR(pExpr (of_list [Id; Add; Id; Mul; Id]) ) );
  run1 ~n:1 (REPR(pExpr (of_list [Id; Add; Id; Add; Id]) ) );
  run1 ~n:1 (REPR(fun q -> pExpr q !(M (!I, !I))         ) );
  ()
