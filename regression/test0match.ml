open Printf
open MiniKanren
open ImplicitPrinters
(*
type pat = Pany
         | Pvar of string
         | Pconstant of int
         | Ptuple of pat list
         | Pconstruct of string * pat option
         | Por of pat * pat

implicit module Show_pat : (SHOW with type t = pat) = struct
    type t = pat
    let rec show = function
      | Pany -> "_"
      | Pvar s -> s
      | Pconstant x -> string_of_int x
      | Ptuple ps -> sprintf "(%s)" @@ String.concat "," @@ List.map show ps
      | Pconstruct (name,None) -> name
      | Pconstruct (name,Some p) -> sprintf "%s %s" name (show p)
      | Por (p1,p2) -> sprintf "%s | %s" (show p1) (show p2)
end

let pat_of_parsetree root =
  let open Longident in
  let open Asttypes in
  let open Parsetree in
  let rec helper p =
    match p.ppat_desc with
    | Ppat_any -> Pany
    | Ppat_var {txt; _} -> Pvar txt
    | Ppat_constant (Const_int n) -> Pconstant n
    | Ppat_or (x,y) -> Por (helper x, helper y)
    | Ppat_construct ({txt=Lident name;_},None)   -> Pconstruct (name, None)
    | Ppat_construct ({txt=Lident name;_},Some x) -> Pconstruct (name, Some (helper x))
    | Ppat_or (p1,p2) -> Por (helper p1, helper p2)
    | Ppat_tuple ps -> Ptuple (List.map helper ps)
    | _ ->
       let b = Buffer.create 20 in
       let fmt = Format.formatter_of_buffer b in
       let () = Pprintast.pattern fmt p in
       let () = Format.pp_print_flush fmt () in
       failwith
         (sprintf "Can't convert this OCaml pattern to mini one:\n" ^ (Buffer.contents b))
  in
  helper root

let () =
  let f x = print_endline @@ show (pat_of_parsetree x) in
  f [%pat? [] ];
  f [%pat? _::_ ];
  f [%pat? 1::2::[] ];
  ()
  *)
module MiniLambda = struct
  type structured_constant = (* Const_int  *)int
  type lambda_switch =
    { sw_numconsts: int;                  (* Number of integer cases *)
      sw_consts: (int * lambda) list;     (* Integer cases *)
      sw_numblocks: int;                  (* Number of tag block cases *)
      sw_blocks: (int * lambda) list;     (* Tag block cases *)
      sw_failaction : lambda option}      (* Action to take if failure *)
  and lambda =
    | Lvar of Ident.t logic
    | Lconst of structured_constant logic
    | Lapply of lambda logic * lambda logic llist
    (* | Lfunction of function_kind * Ident.t list * lambda *)
    | Llet of Lambda.let_kind * Ident.t * lambda logic * lambda logic
    (* | Lletrec of (Ident.t * Lambda.lambda) list * Lambda.lambda *)
    (* | Lprim of Lambda.primitive * Lambda.lambda list *)
    | Lswitch of lambda * lambda_switch
    (* | Lstringswitch of Lambda.lambda * (string * Lambda.lambda) list * *)
    (*                    Lambda.lambda option *)
    | Lstaticraise of int * Lambda.lambda list
    | Lstaticcatch of lambda * (int * Ident.t list) * lambda
    | Ltrywith of lambda * Ident.t * Lambda.lambda
    | Lifthenelse of lambda logic * lambda logic * lambda logic
    | Lsequence of lambda * lambda
    (* | Lwhile of Lambda.lambda * Lambda.lambda *)
    (* | Lfor of Ident.t * Lambda.lambda * Lambda.lambda * *)
    (*         Asttypes.direction_flag * Lambda.lambda *)
    | Lassign of Ident.t * lambda
    (* | Lsend of Lambda.meth_kind * Lambda.lambda * Lambda.lambda * *)
    (*   Lambda.lambda list * Location.t *)
    (* | Levent of Lambda.lambda * Lambda.lambda_event *)
    (* | Lifused of Ident.t * Lambda.lambda *)
end
(*
implicit module Show_Structured_constant : (SHOW with type t=MiniLambda.structured_constant) =
struct
  type t = MiniLambda.structured_constant
  let show =
    let rec helper = function
      | (* MiniLambda.Const_int *) n -> string_of_int n
    in
    helper
end *)
implicit module Show_MiniLambda : (SHOW with type t = MiniLambda.lambda) =
struct
  type t = MiniLambda.lambda
  let show =
    let rec helper = function
      | _ -> "not implemented"
    in
    helper
end

(* let () = print_endline @@ show MiniLambda.(!(Const_int 11)) *)
let is_positive lam =
  fresh (n)
        (lam === !Lconst !n)
        (fun st ->)

exception No_var_in_env
let eval_lambda
    (env: Ident.t logic -> MiniLambda.structured_constant)
    (lam_ast: MiniLambda.lambda) =
  let open MiniLambda in
  let open Tester.M in
  let rec evalo l ans =
    printf "evalo '%s' '%s'\n%!" (show l) (show ans);
    conde
      [ fresh (id1)
          (l   === !(Lvar id1))
          (ans === !(Lconst !(env id1)) )
      ; fresh (_c1) (l === !(Lconst _c1)) &&& (l === ans)
      ; fresh (cond ifb elseb)
          ( l === !(Lifthenelse (cond, ifb, elseb)) )
          (* evaluating condition *)
          (fresh (rez)
                 (evalo cond rez)
                 (* (conde *)
                 (*    [ fresh _d1 *)
                 (*        (rez === !(Lvar _d1)) *)
                 (*        (fun _ -> raise No_var_in_env) *)
                 (*    ; fresh c1 *)
                 (*        (rez === !(Lconst c1)) *)
                 (*        (fun st -> if c1>=0 then (ifb == ans) st else (elseb === ans) st) *)
                 (*    ; (fun _st -> assert false) *)
                 (* ])) *)
                 (fun st ->
                  if not (is_value rez) then failwith "can't evaluate if condition"
                  else
                    match to_value_exn rez with
                    | Lconst (Value (x,_)) ->
                       if x>=0 then (ifb === ans) st else (elseb === ans) st
                    | _ -> failwith "can't evaluate if condition to constant") )
      ]
  in
  let open Tester.M.ConvenienceStream in
  let open ImplicitPrinters in
  let xs = run one (evalo !lam_ast) |> MiniKanren.Stream.take ~n:(-1)
           |> List.map (fun (_logger, (_q,_constraints)) -> _q)
  in
  (* let (_:int list) = xs in *)
  printf "answers: %d\n%!" (List.length xs);
  List.iter (fun x -> print_endline @@ show x) xs


let () =
  let open MiniLambda in
  let env : Ident.t logic -> structured_constant  =
    fun x -> 1
  in

  let lam1 = Lifthenelse (!(Lconst !1), !(Lconst !2), !(Lconst !3)) in
  let lam2 = Lconst !1 in
  let () = eval_lambda env lam2 in
  let () = eval_lambda env lam1 in
  ()

(* let eval_lambda (l: Lambda.lambda) =      *)
(* type right_expr = int *)
(* type match_expr = Pexp_match of (string list * (pat * right_expr) list) *)

(* type token = Id | Add | Mul *)

(* module Show_token_explicit: (SHOW with type t = token) = struct *)
(*   type t = token *)
(*   let show = function *)
(*     | Id -> "Id" *)
(*     | Add -> "Add" *)
(*     | Mul -> "Mul" *)
(* end *)
(* implicit module Show_token = Show_token_explicit *)

(* type expr  = I | A of expr logic * expr logic | M of expr logic * expr logic *)
(* module rec Show_expr_explicit: (SHOW with type t = expr) = struct *)
(*   type t = expr *)
(*   let show = function *)
(*     | I -> "I" *)
(*     | A (l,r) -> *)
(*        sprintf "A (%s, %s)" (Show_expr_logic.show l) (Show_expr_logic.show r) *)
(*     | M (l,r) -> *)
(*        sprintf "M (%s, %s)" (Show_expr_logic.show l) (Show_expr_logic.show r) *)
(* end *)
(* and Show_expr_logic: (SHOW with type t = expr logic) = Show_logic_explicit(Show_expr_explicit) *)
(* implicit module Show_expr = Show_expr_explicit *)

(* open Tester.M *)

(* let (!) = embed *)

(* let sym t i i' = *)
(*   fresh (x xs) *)
(*     (i === x%xs) (t === x) (i' === xs) *)

(* let eof i = i === !(Nil : token llist) *)

(* let (|>) x y = fun i i'' r'' -> *)
(*   fresh (i' r') *)
(*     (x i  i' r') *)
(*     (y r' i' i'' r'') *)

(* let (<|>) x y = fun i i' r -> *)
(*   conde [x i i' r; y i i' r] *)

(* let rec pId i i' r = (sym !Id i i') &&& (r === !I) *)
(* and pAdd i i' r = (pMulPlusAdd <|> pMul) i i' r *)
(* and pMulPlusAdd i i' r = ( *)
(*       pMul |> *)
(*       (fun r i i'' r'' -> *)
(*          fresh (r' i') *)
(*            (sym !Add i i') *)
(*            (r'' === !(A (r, r'))) *)
(*            (pAdd i' i'' r') *)
(*       )) i i' r *)
(* and pMul i i' r = (pIdAstMul <|> pId) i i' r *)
(* and pIdAstMul i i' r= ( *)
(*       pId |> *)
(*       (fun r i i'' r'' -> *)
(*          fresh (r' i') *)
(*            (sym !Mul i i') *)
(*            (r'' === !(M (r, r'))) *)
(*            (pMul i' i'' r') *)
(*       )) i i' r *)
(* and pTop i i' r = pAdd i i' r *)

(* let pExpr i r = fresh (i') (pTop i i' r) (eof i') *)

(* open Tester *)

(* let _ = *)
(*   run1 ~n:1 (REPR(pExpr (of_list [Id])                   ) ); *)
(*   run1 ~n:1 (REPR(pExpr (of_list [Id; Mul; Id])          ) ); *)
(*   run1 ~n:1 (REPR(pExpr (of_list [Id; Mul; Id; Mul; Id]) ) ); *)
(*   run1 ~n:1 (REPR(pExpr (of_list [Id; Mul; Id; Add; Id]) ) ); *)
(*   run1 ~n:1 (REPR(pExpr (of_list [Id; Add; Id; Mul; Id]) ) ); *)
(*   run1 ~n:1 (REPR(pExpr (of_list [Id; Add; Id; Add; Id]) ) ); *)
(*   run1 ~n:1 (REPR(fun q -> pExpr q !(M (!I, !I))         ) ); *)
(*   () *)
