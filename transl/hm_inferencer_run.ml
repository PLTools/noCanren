open Printf
open MiniKanren
open Hm_inferencer
open Tester
open GT

let show_maybe f = function
  | Nothing -> "None"
  | Just x -> Printf.sprintf "Some (%s)" (f x)

let rec show_gnum num =
  let rec helper = function
  | Z -> 0
  | S x -> 1  + (helper x)
  in
  string_of_int @@ helper num

let rec show_lambda_type f = function
  | TInt -> "int"
  | TBool -> "bool"
  | TVar n -> Printf.sprintf "_.(%s)" (f n)
  | TFun (l,r) -> Printf.sprintf "(%s -> %s)" (show_lambda_type f l) (show_lambda_type f r)
  | TPair(l,r) -> Printf.sprintf "(%s * %s)" (show_lambda_type f l) (show_lambda_type f r)

type lam = (string, (int,bool) gliteral, lam) glambda
type llam = (string logic, (int logic, bool logic) gliteral logic, llam) glambda logic
type ilam = (lam, llam) injected


(* let (_:int) = the_true *)
let myshow x = (show_lambda_type show_gnum) x
let show_glambda f1 f2 f3: (_,_,_) glambda -> string = function
  | Var s -> f1 s
  | Lit l -> f2 l
  | Tuple2 (l,r) -> sprintf "(%s, %s)" (f3 l) (f3 r)
  | App (l,r) -> sprintf "(%s)(%s)" (f3 l) (f3 r)
  | Abst (x,t) -> sprintf "(fun %s -> %s)" (f1 x) (f3 t)
  | Let (x,t1,t2) -> sprintf "let %s = %s in %s" (f1 x) (f3 t1) (f3 t2)


let show_gliteral f1 f2 = function
  | LInt n -> f1 n
  | LBool b -> f2 b
let show_literal = show_gliteral (show int) (show bool)
let show_lliteral l =
  GT.show logic (show_gliteral (show logic @@ show int) (show logic @@ show bool)) l
let literal_reifier r = For_gliteral.reify reify reify r

let rec show_lambda  l = show_glambda (GT.show string) show_literal show_lambda l
let rec show_llambda (l: llam) : string =
  show logic
    (show_glambda
       (GT.show logic (GT.show string))
       show_lliteral
       show_llambda)
    l

let rec lambda_reifier: Env.t -> ilam -> llam = fun  l ->
  For_glambda.reify reify literal_reifier lambda_reifier l

let () =
  (* run_exn (fun _ -> ":)") (-1) q  qh  ("true?", (fun q -> the_true q  ) ); *)
  ()


let test0: ilam = var !!"x"
let term0: ilam = abst !!"x"  (var !!"x")
let term1: ilam = let_ (!!"f")  (abst !!"x" (var !!"x"))
    (app (var !!"f")
       (abst !!"x" (app (var !!"f") (var !!"x"))))

(*  (fun f -> f (fun x -> f x)) (fun y -> y) *)
let term2: ilam =
  app (abst !!"f" (app (var !!"f")
                       (abst !!"x" (app (var !!"f") (var !!"x"))) ))
         (abst !!"x" (var !!"x"))

let term3: ilam = tuple2 (lit (lInt !!5)) (lit (lInt !!6))
let term4: ilam =  (lit (lInt !!6))
let term5: ilam =  (lit (lBool !!true))

let () =
  run_exn myshow (-1) q  qh  ("typeof term0", (fun q -> nat_type_inference term0 q  ) );
  run_exn myshow (-1) q  qh  ("typeof term1", (fun q -> nat_type_inference term1 q  ) );
  run_exn myshow (-1) q  qh  ("typeof term2", (fun q -> nat_type_inference term2 q  ) );
  run_exn myshow (-1) q  qh  ("typeof term5", (fun q -> nat_type_inference term5 q  ) );
  run_exn myshow (-1) q  qh  ("typeof term4", (fun q -> nat_type_inference term4 q  ) );
  run_exn myshow (-1) q  qh  ("typeof term3", (fun q -> nat_type_inference term3 q  ) );
  ()

open MiniKanren.Std
open MiniKanren

let peano n =
  conde
    [ n === !!1
    ; n === !!2
    ]
(*
let rec lookupo x env t =
  fresh (rest y v)
    (env === ((Pair.pair y v) % rest))
    (conde [
        (y === x) &&& (v === t);
        (y =/= x) &&& (lookupo x rest t)
      ])

let rec evalo term typ env env_types rez =
  conde
    [ Fresh.one (fun v ->
      (term === var v ) &&& (lookupo v env rez))
    ; fresh (n)
          (term === lit (lInt n) )
          (typ === tInt ())
          (rez === term)
          (peano n)
    ; fresh (f f2 arg arg2 tf targ trez var1 body1)
          (term === app f arg)
          (tf === tFun targ trez)
          (nat_type_inference f   (just tf))
          (nat_type_inference arg (just targ))
          (* evaluate f and arg *)
          (evalo f tf env env_types f2)
          (evalo arg targ env env_types arg2)
          (f2 === abst var1 body1)
          (evalo body1 trez ((Pair.pair var1 arg2) % env) env_types rez )
    ; fresh (v body tv tbody body2)
          (term === abst v body)
          (typ === tFun tv tbody)
          (evalo body tbody env env_types body2)
          (rez === (abst v body2))
    ] *)

let () =
  runR lambda_reifier show_lambda show_llambda (-1) q  qh ("int ids", (fun t ->
        fresh (q) (nat_type_inference t (tFun (tVar q) (tVar q))  ) ));
  (* runR lambda_reifier show_lambda show_llambda (-1) q  qh ("ints", (fun t ->
   *     evalo t (tFun (tInt()) (tInt()) )   (nil ())   (nil ()) t
   *   )); *)
  (* runR lambda_reifier show_lambda show_llambda (-1) q  qh ("ints", (fun t ->
      evalo t (tFun (tInt()) (tInt()) )   (nil ())   (nil ()) t
    )); *)
  ()
