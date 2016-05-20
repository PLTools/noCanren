open Printf
(* open MiniKanren *)
open ImplicitPrinters
(* open Tester.M *)

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

module Value = struct
  type t = Vint of int
         | Vvar of string
         | Vtuple of t list
         | Vconstructor of string * t list

  let is_constructor name = function
    | Vconstructor (n,_) when n = name -> true
    | _ -> false

  (* let is_variable = function Vvar _ -> true | _ -> false *)
  (* let get_varname_exn = function Vvar v -> v | _ -> failwith "bad argument" *)

  let field_exn n : t -> t = function
    | Vint x when n=0 -> Vint x
    | Vvar _
    | Vint _ -> failwith "Bad argument of field_exn"
    | Vconstructor (_,xs) -> List.nth xs n
    | Vtuple xs -> List.nth xs n

  let vpair x y = Vtuple [x;y]
  let vconstructor name xs = Vconstructor (name, xs)

  let print ppf v =
    let wrap_cname = function "::" -> "(::)" | other -> other in
    let open Format in
    let rec helper = function
      | Vvar s -> fprintf ppf "%s" s
      | Vint n -> fprintf ppf "%d" n
      | Vtuple xs ->
         let nocomma = ref true in
         fprintf ppf "(";
         List.iter (fun p ->
                    if !nocomma then nocomma := false else fprintf ppf "@,,@ ";
                    helper p
                   ) xs;
         fprintf ppf ")"
      | Vconstructor (name, []) -> fprintf ppf "%s" (wrap_cname name)
      | Vconstructor (name, xs) ->
         fprintf ppf "@[@,%s@ " name;
         helper (Vtuple xs);
         fprintf ppf "@]"
    in
    helper v

  let nil = Vconstructor ("[]",[])
  let cons h tl = Vconstructor ("::",[h;tl])
end

let vpair a b = Value.Vtuple [a;b]

type varname = string

module Pat = struct
  type t = Pany
         | Pvar of varname
         | Pconstant of int
         | Ptuple of t list
         | Pconstructor of string * t list
         (* | Por of pat * pat *)
  let is_variable = function Pvar _ -> true | _ -> false
  let get_varname_exn = function Pvar name -> name | _ -> failwith "bad argument"

  let is_constructor = function Pconstructor _ -> true | _ -> false
  let get_const_info_exn = function
    | Pconstructor (name,xs) -> name,xs
    | _ -> failwith "bad argument"

  type kind = Kany | Kvar | Kconst | Ktuple | Kconstructor
  let get_kind = function
    | Pany -> Kany
    | Pvar _ -> Kvar
    | Pconstant _ -> Kconst
    | Ptuple _ -> Ktuple
    | Pconstructor _ -> Kconstructor

  let show_kind = function
    | Kany -> "Kany"
    | Kvar -> "Kvar"
    | Kconst -> "Kconst"
    | Kconstructor -> "Kconstructor"
    | Ktuple -> failwith "tuples should be removed"

  let show p =
    let rec helper = function
      | Pany -> "Pany"
      | Pvar s -> sprintf "(Pvar %s)" s
      | Pconstant n -> string_of_int n
      | Ptuple xs -> sprintf "(%s)" (String.concat "," @@ List.map helper xs)
      | Pconstructor(name,xs) ->
         sprintf "%s(%s)" name (String.concat "," @@ List.map helper xs)
    in
    helper p
end

type pat = Pat.t

implicit module Show_pat : (SHOW with type t = Pat.t) = struct
    open Pat
    type t = pat
    let rec show = function
      | Pany -> "_"
      | Pvar s -> s
      | Pconstant x -> string_of_int x
      | Ptuple ps -> sprintf "(%s)" @@ String.concat "," @@ List.map show ps
      | Pconstructor (name,xs) -> sprintf "%s %s" name (show (Ptuple xs))
      (* | Por (p1,p2) -> sprintf "%s | %s" (show p1) (show p2) *)
end


let ppair x y = Pat.Ptuple [x;y]
let pconstructor name xs = Pat.Pconstructor (name,xs)

type source_program = Value.t * (Pat.t * Value.t) list
(*
let pat_of_parsetree root =
  let open Longident in
  let open Asttypes in
  let open Parsetree in
  let rec helper p =
    match p.ppat_desc with
    | Ppat_any -> Pany
    | Ppat_var {txt; _} -> Pvar txt
    | Ppat_constant (Const_int n) -> Pconstant n
    (* | Ppat_or (x,y) -> Por (helper x, helper y) *)
    | Ppat_construct ({txt=Lident name;_},None)   -> Pconstructor (name, None)
    | Ppat_construct ({txt=Lident name;_},Some x) -> Pconstructor (name, Some (helper x))
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

(*
let (!) = embed

module Nat = struct
  type t = O | S of t logic
  let show = function
    | O -> "O"
    | S n -> sprintf "S (%s)" (show_logic_naive n)
end
implicit module Show_nat : (SHOW with type t = Nat.t) = Nat

let nat_of_int n : Nat.t =
  if n<0 then failwith "bad argument"
  else
    let rec helper acc n =
      if n=0 then acc
      else helper (Nat.S !acc) (n-1)
    in
    helper Nat.O n

let is_positive_nat n = fresh (_zero) (n === !(Nat.S _zero))
let is_nonnegative_nat n = fresh (_zero) (conde [(n === !(Nat.S _zero));  (n === !Nat.O) ])

module Peano_int = struct
    type t = bool * Nat.t logic
    let show : t -> string = fun (p,n) ->
      if p then show n
      else "-" ^ (show n)
    let of_int n =
      if n>=0 then (true, !(nat_of_int n) )
      else (false, !(nat_of_int (-n)) )
end
(*implicit module Show_peano_int: (SHOW with type t = Peano_int.t) = Peano_int*)

let is_positive_peano p =
  let open Nat in
  fresh (_zero) (p === !(true, !(S _zero)) )

let is_negative_peano p =
  let open Nat in
  fresh (_zero) (p === !(false, !(S _zero)) )

let is_non_negative_peano p =
  let open Nat in
  conde [ p === !(true, !O)
        ; p === !(false, !O)
        ; is_positive_peano p
        ]

let is_non_positive_peano p =
  let open Nat in
  conde [ p === !(true, !O)
        ; p === !(false, !O)
        ; is_negative_peano p
        ]
 *)
(*
module MiniLambda = struct
  type structured_constant = (* Const_int  *) Peano_int.t
  type lambda_switch =
    { sw_numconsts: int;                  (* Number of integer cases *)
      sw_consts: (int * lambda) list;     (* Integer cases *)
      sw_numblocks: int;                  (* Number of tag block cases *)
      sw_blocks: (int * lambda) list;     (* Tag block cases *)
      sw_failaction : lambda option}      (* Action to take if failure *)
  and lambda =
    | Lvar of Ident.t logic
    | Lconst of structured_constant logic
    (* | Lapply of lambda logic * lambda logic llist *)
    (* | Lfunction of function_kind * Ident.t list * lambda *)
    (* | Llet of Lambda.let_kind * Ident.t * lambda logic * lambda logic *)
    (* | Lletrec of (Ident.t * Lambda.lambda) list * Lambda.lambda *)
    (* | Lprim of Lambda.primitive * Lambda.lambda list *)
    (* | Lswitch of lambda * lambda_switch *)
    (* | Lstringswitch of Lambda.lambda * (string * Lambda.lambda) list * *)
    (*                    Lambda.lambda option *)
    (* | Lstaticraise of int * Lambda.lambda list *)
    (* | Lstaticcatch of lambda * (int * Ident.t list) * lambda *)
    (* | Ltrywith of lambda * Ident.t * Lambda.lambda *)
    | Lifthenelse of (lambda logic * lambda logic * lambda logic)
    (* | Lsequence of lambda * lambda *)
    (* | Lwhile of Lambda.lambda * Lambda.lambda *)
    (* | Lfor of Ident.t * Lambda.lambda * Lambda.lambda * *)
    (*         Asttypes.direction_flag * Lambda.lambda *)
    (* | Lassign of Ident.t * lambda *)
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
    let open MiniLambda in
    let rec helper = function
      (* | Lconst ((Var _) as l) -> sprintf "Lconst %s" (show_logic_naive l) *)
      | Lconst l -> sprintf "Lconst %s" (show_logic_naive l)
      | Lifthenelse (cond,ifb,elseb) -> sprintf "if %s then %s else %s fi" (show_logic_naive cond) (show_logic_naive ifb) (show_logic_naive elseb)
      | _ -> "<not implemented XXX>"
    in
    helper
end

module type INTABLE = sig
    type t
    val of_int : t -> Peano_int.t
end
implicit module Int_as_intable : (INTABLE with type t = int) = struct
  type t = int
  let of_int = Peano_int.of_int
end
implicit module Peano_as_intable : (INTABLE with type t = Peano_int.t) = struct
  type t = Peano_int.t
  let of_int n = n
end

let make_const {X: INTABLE} (n:X.t) : MiniLambda.structured_constant = X.of_int n

let is_positive_const lam =
  let open MiniLambda in
  fresh (n)
        (lam === !(Lconst n))
        (is_positive_peano n)

let is_nonnegative_const lam =
  let open MiniLambda in
  fresh n
    (lam === !(Lconst n))
    (is_non_negative_peano n)

let is_negative_const lam =
  let open MiniLambda in
  fresh n
    (lam === !(Lconst n))
    (is_negative_peano n)

let is_non_positive_const lam =
  let open MiniLambda in
  fresh n
    (lam === !(Lconst n))
    (is_non_positive_peano n)

(* let () = print_endline @@ show MiniLambda.(!(Const_int 11)) *)

exception No_var_in_env
let eval_lambda
    (env: Ident.t logic -> MiniLambda.structured_constant)
    (lam_ast: MiniLambda.lambda) =
  let open MiniLambda in
  let open Tester.M in
  let rec evalo l (ans: MiniLambda.lambda logic) =
    printf "evalo '%s' '%s'\n%!" (show l) (show ans);

    conde
      [ fresh (id1)
          (l   === !(Lvar id1))
          (ans === !(Lconst !(env id1)) )
      ; fresh (_c1) (l === !(Lconst _c1)) &&& (l === ans)
      ; fresh (cond ifb elseb) (
          ( l === !(Lifthenelse (cond, ifb, elseb)) ) &&&
          (* evaluating condition *)
          (fresh (rez)
                 (evalo cond rez)
                 (conde
                    [ (is_positive_const rez) &&& (ifb === ans)
                    ; (is_negative_const rez) &&& (elseb === ans)
                    ])) )
      ]
  in
  let open Tester.M.ConvenienceStream in
  let open ImplicitPrinters in
  (* let stream = run one @@ evalo !(Lconst !(make_const 1)) in *)
  let stream = run one @@ evalo !lam_ast in
  (* printf "stream = '%s'\n%!" (MiniKanren.generic_show stream); *)
  let _ = MiniKanren.Stream.take ~n:1 stream in

  let xs = stream
           |> MiniKanren.Stream.take ~n:1
           |> List.map (fun (_logger, (_q,_constraints)) -> _q)
  in
  (* let xs = stream (fun var1 -> var1 1 |> List.map (fun (_logger, (_q,_constraints)) -> _q) ) *)
  (* in *)
  (* let (_:int list) = xs in *)
  (* let (_q,stream) = Tester.M.run (call_fresh (fun q st ->  evalo !(Lconst !(make_const 1)) q st,q) ) in *)
  (* let _ = Stream.take ~n:1 stream in *)
  printf "answers: %d\n%!" (List.length xs);
  List.iter (fun x -> print_endline @@ show x) xs

let () =
  let open MiniLambda in
  let env : Ident.t logic -> structured_constant  =
    fun x ->
      Peano_int.of_int 1
  in

  let lam1 = Lifthenelse ( !(Lconst !(make_const 1))
                         , !(Lconst !(make_const (Peano_int.of_int 2)) )
                         , !(Lconst !(make_const 3))
                         ) in
  let lam2 = Lconst !(make_const 1) in

  (* let () = eval_lambda env lam2 in *)
  let () = eval_lambda env lam1 in
  ()
  *)

let eval_match (what: Value.t) pats  =
  let open Pat in
  let distinct_sets : 'a list -> 'a list -> bool = fun xs ys ->
    let rec helper = function
    | [] -> true
    | x::xs when List.mem x ys -> false
    | _::xs -> helper xs
    in
    helper xs
  in
  let merge_subs xs ys =
    if distinct_sets xs ys then List.append xs ys
    else failwith "not distinct sets"
  in
  (* `what` should be tuple with no free variables *)
  let rec match_one what patt =
    let open Value in
    match what,patt with
    (* | _ , Por _ -> failwith "or pattern not implemented" *)
    | Vint _, Pany -> Some []
    | Vint n, Pvar name -> Some [ (name,Vint n) ]
    | Vint n, Pconstant m when n=m -> Some []
    | Vint _, Pconstant _ -> None
    | Vint _, Ptuple _
    | Vint _, Pconstructor _ -> None
    | Vtuple xs, Ptuple ys when List.length xs <> List.length ys -> None
    | Vtuple xs, Ptuple ys ->
       List.combine xs ys
       |> ListLabels.fold_left
            ~init:(Some [])
            ~f:(fun acc (x,y) ->
                                   match acc with
                                   | Some subs -> begin
                                       match match_one x y with
                                       | Some subs2 when distinct_sets subs subs2 ->
                                          Some (merge_subs subs subs2)
                                       | _ -> failwith "can't merge subs  in matching tuple"
                                     end
                                   | None -> None)
    | Vtuple _,_ -> None
    | Vconstructor (name,_), Pconstructor (name2,_) when name<>name2 ->
       None
    (* | Vconstructor (_,[x]), Pconstructor(_,Some p) -> match_one x p *)
    (* | Vconstructor (_,[x]), Pconstructor(_,None)   -> None *)
    | Vconstructor (_,xs),  Pconstructor(_, ys) ->
       match_one (Vtuple xs) (Ptuple ys)
    (* | Vconstructor _, Pconstructor _ -> None *)
    | Vconstructor _, _ -> None
    | Vvar _,_ -> failwith "TODO: not implemented"
  in
  let module S = struct
      type subs = (string * Value.t) list
      exception Answer of subs * Value.t
  end in
  try List.iter (fun (pat, right) ->
                match match_one what pat with
                | Some subs -> raise (S.Answer (subs,right))
                | None -> ()) pats;
      None
 with S.Answer (subs, right) -> Some right



let test_eval_match () =
  let open Value in

  assert (eval_match (Vint 1) [ (Pany, Vint 1) ] = Some (Vint 1) );
  assert (eval_match (Vint 1) [ (Pvar "x", Vint 1) ] = Some (Vint 1) );
  assert (eval_match (Vint 1) [ (Pvar "x", Vint 1) ] = Some (Vint 1) );
  assert (eval_match (Vtuple [Vint 1;Vint 2]) [ (Pvar "x", Vint 1) ] = None );
  assert (eval_match (Vtuple [Vint 1;Vint 2])
                     [ (Ptuple [Pvar "x"; Pconstant 2], Vint 3) ] = Some (Vint 3) );
  assert (eval_match  (Vtuple [Vconstructor("Some", [Vint 1]); Vint 2])
                     [(Ptuple [Pconstructor("Some", [Pany]); Pconstant 2], Vint 3) ] = Some (Vint 3) );
  assert (eval_match  (Vconstructor("Some", [Vint 1]))
                     [ Pconstructor("Some", [])    , Vint 666
                     ; Pconstructor("Some", [Pany]), Vint 777
                     ]
         = Some (Vint 777) );
  ()

let () = test_eval_match ()

module MiniLambda_Nologic = struct
  type structured_constant = int
  type ident = string
  type lambda_switch =
    { sw_numconsts: int;                  (* Number of integer cases *)
      sw_consts: (int * lambda) list;     (* Integer cases *)
      sw_numblocks: int;                  (* Number of tag block cases *)
      sw_blocks: (int * lambda) list;     (* Tag block cases *)
      sw_failaction : lambda option}      (* Action to take if failure *)
  and lambda =
    | Lvar of ident
    | Lconst of Value.t
    | Lfield of int * ident
    | Lcheckconstr of string * varname
    | Lneq of ident * Value.t
    | Llet of ident * lambda * lambda
    | Lifthenelse of lambda * lambda * lambda
    | Lswitchconstr of lambda * (string * lambda) list * lambda option
    | Lstaticraise of int (* * Lambda.lambda list *)
    | Lstaticcatch of lambda * (int option) * lambda
    (* Lstaticcatch (l1, None, l2) stands for
         catch lam1 with lam2
       and Lstaticcatch (l1, Some n, l2) ---
         catch lam1 width n -> l2
    *)
    (* | Ltrywith of lambda * Ident.t * Lambda.lambda *)
    (* | MatchFailure *)
    (* | Lletrec of (Ident.t * Lambda.lambda) list * Lambda.lambda *)
    (* | Lprim of Lambda.primitive * Lambda.lambda list *)
    (* | Lswitch of lambda * lambda_switch *)
    (* | Lstringswitch of Lambda.lambda * (string * Lambda.lambda) list * *)
    (*                    Lambda.lambda option *)
    (* | Lapply of lambda logic * lambda logic llist *)
    (* | Lfunction of function_kind * Ident.t list * lambda *)
    (* | Lsequence of lambda * lambda *)
    (* | Lwhile of Lambda.lambda * Lambda.lambda *)
    (* | Lfor of Ident.t * Lambda.lambda * Lambda.lambda * *)
    (*         Asttypes.direction_flag * Lambda.lambda *)
    (* | Lassign of Ident.t * lambda *)
    (* | Lsend of Lambda.meth_kind * Lambda.lambda * Lambda.lambda * *)
    (*   Lambda.lambda list * Location.t *)
    (* | Levent of Lambda.lambda * Lambda.lambda_event *)
    (* | Lifused of Ident.t * Lambda.lambda *)

  let () = ()
  let lfield n ident = Lfield(n, ident)
  let lcheckconstr str varname = Lcheckconstr (str, varname)
  let lneq ident value_ = Lneq (ident, value_)
  let llet name bnd where_ = Llet (name, bnd, where_)
  let lstaticcatch try_block n handler = Lstaticcatch(try_block, n, handler)
  let lifthenelse cond trueb elseb = Lifthenelse (cond, trueb, elseb)
  let lswitchconstr ?default what cases = Lswitchconstr(what, cases, default)

  open Format

  let rec print ppf root =
    let pr fmt = fprintf ppf fmt in
    let rec helper = function
    | Lvar s   -> fprintf ppf "%s" s
    | Lconst c -> Value.print ppf c
    | Lfield (n, ident) -> fprintf ppf "@[get_field@ %d@ %s@ @]" n ident
    | Lcheckconstr (cname, vname) -> fprintf ppf "@[check_constr@ \"%s\"@ %s@ @]" cname vname
    | Lneq (varname, val_) -> pr "@[%s@ <>@ %a@,@]" varname Value.print val_
    | Llet (ident, lam1, lam2) ->
       pr "@[let %s =@ " ident;
       helper lam1;
       pr "@ in @,";
       helper lam2;
       pr "@ @]"
    | Lstaticraise n -> pr "@[exit %d@]" n
    | Lstaticcatch (what, n, handler) ->
       pr "@[try@ ";
       helper what;
       pr "with@%s ->@ " (Option.map ~f:(sprintf " %d") ~default:"" n);
       helper handler;
       pr "@]"
    | Lifthenelse (cond,trueb,elseb) ->
       pr "@[if@ ";
       helper cond;
       pr "@ then@ ";
       helper trueb;
       pr "@ else@ ";
       helper elseb;
       pr "@]"
    | Lswitchconstr (expr, cases, else_case) ->
       pr "@[sw_constr ";
       helper expr;
       pr " with@ ";
       List.iter (fun (cname,lam) -> pr "@[case %s :@ %a@]" cname print lam) cases;
       let () = match else_case with
         | Some lam -> pr "_ -> %a" print lam;
         | None -> ()
       in
       pr "@]"
    in
    helper root;
    fprintf ppf "@."

  let show lam =
    let b = Buffer.create 10 in
    let fmt = Format.formatter_of_buffer b in
    print fmt lam;
    pp_print_flush fmt ();
    Buffer.contents b

  module RezMonad = struct
    type t = [`Ok of Value.t | `Raise of int ]
    let (>>=) x f = match x with
      | `Ok v -> f v
      | `Raise n -> `Raise n
    let map x f = match x with
      | `Ok v -> `Ok (f v)
      | `Raise n -> `Raise n
  end

  let eval root =
    let open Value in
    let open RezMonad in
    let extend_env ~env name val_ = function
      | s when s = name -> val_
      | s -> env s
    in

    let lookup_env env name=
      let rec helper name env =
        match env name with
        | Vvar s when s = name -> failwith "cycle in variable bindings"
        | Vvar n -> helper n env
        | x -> x
      in
      helper name env
    in
    let rec helper env = function
      | Lconst v -> `Ok v
      | Lvar name -> `Ok (lookup_env env name)
      | Lfield (n, name) -> `Ok (Value.field_exn n @@ lookup_env env name)
      | Lneq (name, v) -> `Ok (if lookup_env env name <> v then Vint 1 else Vint 0)
      | Llet (ident, what, where_) ->
         (helper env what) >>= fun bnd -> helper (extend_env ~env ident bnd) where_

      | Lifthenelse (cond,trueb,falseb) -> begin
          match helper env cond with
          | `Raise n -> `Raise n
          | `Ok (Vint n) when n > 0 -> helper env trueb
          | `Ok (Vint n)  -> helper env falseb
          | _ -> failwith "how to evaluate this?"
        end
      | Lstaticraise n -> `Raise n
      | Lstaticcatch (lam, n, handler) -> begin
          match helper env lam with
          | `Ok x -> `Ok x
          | `Raise m when n=None   -> helper env handler
          | `Raise m when n=Some m -> helper env handler
          | `Raise m -> `Raise m
        end
      | Lcheckconstr (constr, varname) ->
         `Ok Value.(if is_constructor constr @@ lookup_env env varname
                    then Vint 1 else Vint 0)
      | Lswitchconstr (lam, [], default) ->
         (helper env lam) >>= fun what -> begin
           match default with
           | None -> `Raise 666
           | Some lam -> helper env lam (* >>= fun ans -> `Ok (ans) *)
         end
      | Lswitchconstr (lam, (cname,right)::cs, default) ->
         (helper env lam) >>= fun what ->
         if is_constructor cname what then helper env right
         else helper env (Lswitchconstr (lam, cs, default))
  in
  let env_exn name = failwith (sprintf "variable %s is not in env" name) in
  helper env_exn root

  let eval_exn root =
    match eval root with
    | `Ok x -> x
    | _ -> failwith "can't evaluate"
end

let test_eval_minilambda_nologic () =
  let open Value in
  let open MiniLambda_Nologic in
  assert (eval_exn (Lconst (Vint 5)) = Vint 5);
  assert (eval_exn (Llet ("x", Lconst (Vint 5), Lconst (Vint 6))) = Vint 6);
  assert (eval_exn (Llet ("x", Lconst (Vint 5), Lvar "x")) = Vint 5);
  ()

let () = test_eval_minilambda_nologic ()


module NaiveCompilation = struct
    open MiniLambda_Nologic
    open Value
    exception Cant_compile

    module Env = struct
      type t = (string * Value.t) list
      let lookup_exn name t = List.assoc name t
      let extend name value_ env = (name, value_) :: env
      let empty : t = []
    end

    (* straightforward compilation to MiniLambda_Nologic*)
    let compile: source_program -> MiniLambda_Nologic.lambda = fun (what,patts) ->
      let open Value in
      let open Pat in
      let next_varname =
        let counter = ref 0 in
        fun () -> incr counter; sprintf "x%d" counter.contents
      in
      let root_varname = "rootVar" in

      let next_exit_counter =
        let exit_counter = ref 0 in
        fun () -> incr exit_counter; exit_counter.contents
      in
      let rec match_one_patt varname patt right elseb =
        match patt with
        | Pany -> right
        | Pvar newname -> Llet (newname, Lvar varname, right)
        | Pconstant c -> Lifthenelse (Lneq (varname,Vint c),  elseb, right)
        | Pconstructor (cname,[]) ->
           lswitchconstr (Lvar varname) [ cname, right ] ~default: elseb
        | Pconstructor (cname,xs) ->
           let exit_code = next_exit_counter () in
           let myexit = Lstaticraise exit_code in
           lstaticcatch
             (lswitchconstr
                (Lvar varname)
                [ (cname, match_one_patt varname (Ptuple xs) right myexit) ]
                ~default:myexit)
             (Some exit_code)
             elseb

(*  (* naive code generation *)
        | Pconstructor (name,[]) ->
           lifthenelse (lcheckconstr name varname) right elseb
        | Pconstructor (name,xs) ->
           (* naive, with if statement*)
           let exit_code = next_exit_counter () in
           Lstaticcatch(
             Lifthenelse (Lcheckconstr (name, varname),
                          match_one_patt varname (Ptuple xs) right (Lstaticraise exit_code),
                          (Lstaticraise exit_code)),
             exit_code,
             elseb)
 *)
        | Ptuple [a] ->
           let var_a = next_varname () in
           llet var_a (lfield 0 varname) @@
             match_one_patt var_a a right elseb
        | Ptuple [a;b] ->
           let var_a = next_varname () in
           let var_b = next_varname () in
           let exit_code = next_exit_counter () in

           llet var_a (lfield 0 varname) @@
             llet var_b (lfield 1 varname) @@
               lstaticcatch (match_one_patt
                               var_a a
                               (match_one_patt var_b b right (Lstaticraise exit_code))
                               (Lstaticraise exit_code))
                             (Some exit_code)
                             elseb

        | Ptuple _ -> failwith "Ptuple not implemented"
        (* | _ -> Lconst (Vint 0) *)
      in
      (* let start_env = Env.(extend root_varname what empty) in *)
      (* List.fold_right (fun (pat,r) acc -> match_one_patt start_env root_varname pat r acc) *)
      (*                 (List.map (fun (p,r) -> (p,Lconst r)) patts) *)
      (*                 (Lstaticraise 666) *)
      let body =
        List.fold_right (fun (pat,r) acc -> match_one_patt root_varname pat r acc)
          (List.map (fun (p,r) -> (p,Lconst r)) patts)
          (Lstaticraise 666)
      in
      Llet (root_varname, Lconst what, body)


end

exception BadResult of Value.t (* MiniLambda_Nologic.lambda *)
let ___ () =
  let open Value in
  let open MiniLambda_Nologic in
  let foo ?heading  what patts rez =
    let lam = NaiveCompilation.compile (what,patts) in
    let lam =
      match heading with
      | Some h -> h lam
      | None -> lam
    in
    MiniLambda_Nologic.print Format.std_formatter lam;
    print_endline "";
    match eval lam with
    | `Ok r when r=rez -> ()
    | `Ok r -> raise (BadResult r)
    | `Raise n -> failwith (sprintf "unexpected raise %d" n)
    (* assert (eval lam = `Ok rez) *)
  in

  foo (Vint 5) [Pany, Vint 1] (Vint 1);
  foo (Vint 5) [Pvar "x", Vint 1] (Vint 1);
  foo (vpair (Vint 5)(Vint 6)) [ppair (Pvar "x") (Pvar "y"), Vint 1] (Vint 1);

  foo (vpair (Vvar "lx") (Vvar "ly"))
      [ ppair (pconstructor "[]" []) Pany, Vint 1
      ; ppair Pany (pconstructor "[]" []), Vint 2
      ; ppair (pconstructor "::" [Pvar "x"; Pvar "xs"]) (pconstructor "::" [Pvar "y"; Pvar "ys"]), Vint 3
      ]
      (Vint 1)
      ~heading:(fun l -> llet "lx" (Lconst nil) @@ llet "ly" (Lconst nil) l) ;

  ()

module NaiveCompilationWithMatrixes = struct
  open MiniLambda_Nologic
  open Value

  module StringMultiMap (* : (Map.S with type 'a t = ('a list) t) *) = struct
    module M = Map.Make(String)
    include M

    let add key (v: 'a) (map: ('a list) M.t)  =
      try let xs = M.find key map in
          M.add key (v::xs) map
      with Not_found -> M.add key [v] map

    let find_exn = M.find
    let bindings m = List.map ~f:(fun (key, xs) -> (key, List.rev xs)) @@ M.bindings m

  end

  module Matrix = struct
    type t = (pat list * lambda) list

    let iter ~f m = List.iter (fun (pats,right) -> f (List.hd pats) (List.tl pats) right) m
    let check_matrix m =
      let lens = List.map List.length m in
      match lens with
      | [] -> ()
      | x::xs -> assert (List.for_all ((=)x) xs)

    let width = function
      | [] -> 0
      | (ps,r) :: _ -> List.length ps
    let height = List.length

    let is_empty m = List.for_all (fun (x,_) -> x=[]) m
    let first_column m = List.map (fun (xs,_lambda) -> List.hd xs) m

    let check_1st_column cond m =
      List.(for_all cond @@ first_column m)

    let eval_prefix_len cond m =
      let rec helper n = function
        | [] -> n
        | x:: xs when cond x -> helper (n+1) xs
        | _ -> n
      in
      helper 0 @@ first_column m

    let top_left_pat m = m |> List.hd |> fst |> List.hd

    let print m =
      List.iter (fun (xs, r) ->
                 printf "(%s) -> ???\n%!" (String.concat "," @@ List.map Pat.show xs))
                m

    let cut_horizontally n m =
      printf "n = %d\n%!" n;
      print m;
      if n = height m then failwith "This cut will provide empty matrix";
      (* assert (height m = List.length rs); *)
      assert (n>0 && n < height m);
      (List.take ~n m, List.skip ~n m)

    let cut_first_column m =
      List.map ~f:(fun (xs, r) -> (List.tl xs, r)) m

    let classify_prefix (m: t) =
      assert (not (is_empty m));
      let basic_kind = (List.hd @@ fst @@ List.hd m) |>  Pat.get_kind in
      let n = eval_prefix_len (fun pat -> Pat.get_kind pat = basic_kind) m in
      assert (n>=1);
      if n = height m then (basic_kind, m, None)
      else
        let top,bot = cut_horizontally n m in
        (basic_kind, top, Some bot)

    let fold ~f acc x = List.fold_left ~f ~init:acc x

  end


  let group_constrs_in_matrix matrix r =
    let open Pat in
    let module M = Map.Make(String) in
    let map = ref M.empty in
    List.iter2 (fun ps r ->
                match ps with
                | (Pconstructor (name,args))::xs ->
                   let new_data =
                     try let ys = M.find name map.contents in
                         (args, xs, r) :: ys
                     with Not_found -> (args, xs, r) :: []
                   in
                   map := M.add name new_data map.contents
               | _ -> failwith "bad argument"
               ) matrix r;
    M.bindings map.contents
    (* |> List.map (fun (name,moreinfo) *)
    (* () *)

(*
  let check_1st_column_wrap checker getter m =
    if Matrix.check_1st_column checker m && not (Matrix.is_empty m)
    then
      let temp = m |> List.map (function x::xs -> (getter x, xs)
                                       | [] -> failwith "bad argument")
      in
      Some (List.split temp)
    else None

  let check_first_variables m : (varname list * Matrix.t) option =
    check_1st_column_wrap Pat.is_variable Pat.get_varname_exn m

  let check_first_constructors m : ((string * Pat.t list) list * Matrix.t) option =
    check_1st_column_wrap Pat.is_constructor Pat.get_const_info_exn m
    *)
  type input_data = Value.t list * Matrix.t * lambda list
  type state = input_data * input_data option
  let (++) (start,rez) f =
    match rez with
    | Some _ -> (start,rez)
    | None -> (start, f start)

  (* let (_:int)  = (++) *)

  let rec compile : tuple:string list -> matrix: Matrix.t  -> lambda
    = fun ~tuple ~matrix ->
    (* 1st column of matrix is List.map List.hd_exn matrix *)
    assert (List.length tuple = Matrix.width matrix);
    let open Option in
    let open Matrix in

    let next_varname =
      let counter = ref 0 in
      fun () -> incr counter; sprintf "x%d" counter.contents
    in
    (* let next_exit_counter = *)
    (*   let exit_counter = ref 0 in *)
    (*   fun () -> incr exit_counter; exit_counter.contents *)
    (* in *)

    let rec main tuple matrix onfail =
      (* print_endline "calling main for matrix"; *)
      (* Matrix.print matrix; *)
      if Matrix.width matrix = 0 && Matrix.height matrix = 1
      then snd @@ List.hd matrix
      else
      match Matrix.classify_prefix matrix with
      | (Pat.Kconstructor, top, None)  ->
         (* print_endline "only constructors"; *)
         (* assert false *)
         only_constructors tuple top onfail
      | (Pat.Kconstructor, top, Some bot) ->
         only_constructors tuple top (main tuple bot onfail)
         (* assert false *)
      | (Pat.Kvar, top, None)
      | (Pat.Kany, top, None)  ->
         main (List.tl tuple) (Matrix.cut_first_column top) onfail
      | (Pat.Kvar, top, Some bot)
      | (Pat.Kany, top, Some bot)  ->
         main (List.tl tuple) (Matrix.cut_first_column top)
           @@ main tuple bot onfail

      | (pat,_,_) -> failwith
                       (sprintf "some cases in `main` are not done: %s"
                                (Pat.show @@ Matrix.top_left_pat matrix))

    and only_constructors tuple matrix (tail: lambda) =
      let splitted =
      Matrix.fold StringMultiMap.empty matrix
                  ~f:(fun acc -> function
                              | (Pat.Pconstructor(name,args)) :: ps, right ->
                                 StringMultiMap.add name (args, ps, right) acc
                              | _ -> assert false)

      in
      let other_vars = List.tl tuple in
      let main_var = List.hd tuple in
      let splitted = StringMultiMap.bindings splitted in
      List.fold_left ~init:tail splitted
                     ~f:(fun acc (name, lst) ->
                         lifthenelse
                           (lcheckconstr name main_var)
                           (let args_count = List.hd_exn lst |> fst3 |> List.length in
                            let new_vars = List.init args_count (fun _ -> next_varname ()) in

                            let with_vars init =
                              let indexed = List.mapi (fun n x -> (n,x)) new_vars in
                              List.fold_left indexed ~init
                                ~f:(fun acc (n,vname) -> llet vname (lfield n main_var) acc)
                            in

                            let new_matrix : Matrix.t =
                              List.map lst ~f:(fun (args,ps,right) -> (args@ps, right))
                            in
                            let new_tuple = new_vars @ other_vars in
                            with_vars @@ main new_tuple new_matrix acc
                           )
                           acc
                        )
    in
    main tuple matrix (Lconst (Vint 666))

(*
 *)
end

module SimplifyMiniLambda = struct
  open MiniLambda_Nologic
  module StringSet = Set.Make(String)

  let kill_variables lam =
    let module SS = StringSet in
    let rec helper what =
      match what with
      | Lvar name
      | Lfield (_,name)
      | Lcheckconstr (_,name)
      | Lneq (name,_)
      | Lconst (Vvar name) -> (SS.add name SS.empty, what)
      | Lconst _ -> (SS.empty, what)
      | Llet (name, what, where_) ->
         let (s1, what') = helper what in
         let s2, where' = helper where_ in
         if SS.mem name s2 then (SS.union s1 s2, Llet (name, what', where'))
         else (s2, where')
      | Lifthenelse (cond, trueb, elseb) ->
         let s1,cond' = helper cond in
         let s2,trueb' = helper trueb in
         let s3,elseb' = helper elseb in
         (SS.union s1 (SS.union s2 s3), Lifthenelse(cond', trueb', elseb') )
      | Lswitchconstr (lam, cases, default) ->
         let s1,lam' = helper lam in
         let s2,cases' =
           List.fold_left ~init:(SS.empty, []) cases
                          ~f:(fun (set,ls) (name,action) ->
                              let set1,action' = helper action in
                              (SS.union set set1, (name,action') :: ls) )
         in
         let cases' = List.rev cases' in
         let s3,default' = match default with
           | None -> SS.empty, None
           | Some l -> let (s,l') = helper l in (s, Some l')
         in
         (SS.(union (union s1 s2) s3), Lswitchconstr (lam', cases', default'))
      | Lstaticraise n -> (SS.empty, what)
      | Lstaticcatch (lam, exc, handler) ->
         let s1,lam' = helper lam in
         let s2,handler' = helper handler in
         (SS.union s1 s2, lstaticcatch lam' exc handler')

    in
    snd @@ helper lam





end

let ___ () =
  let open Value in
  let open MiniLambda_Nologic in
  let foo ?heading  what patts rez =
    let lam = NaiveCompilationWithMatrixes.compile ~tuple:what ~matrix:patts in
    let lam =
      match heading with
      | Some h -> h lam
      | None -> lam
    in
    let lam = SimplifyMiniLambda.kill_variables lam in
    MiniLambda_Nologic.print Format.std_formatter lam;
    print_endline "";
    match eval lam with
    | `Ok r when r=rez -> ()
    | `Ok r -> raise (BadResult r)
    | `Raise n -> failwith (sprintf "unexpected raise %d" n)
    (* assert (eval lam = `Ok rez) *)
  in

  foo [ "lx"; "ly"]
      [ [pconstructor "[]" []; Pany], Lconst (Vint 1)
      ; [Pany; pconstructor "[]" []], Lconst (Vint 2)
      ; [ pconstructor "::" [Pvar "x"; Pvar "xs"]
        ; pconstructor "::" [Pvar "y"; Pvar "ys"]
        ], Lconst (Vint 3)
      ]
      (Vint 1)
      ~heading:(fun l -> llet "lx" (Lconst nil) @@ llet "ly" (Lconst nil) l) ;

  ()


open MiniKanren
open Tester.M

let (!) = embed

module PatLogic = struct
  (* понадобился для classify line *)
  type t = Pany
         | Pvar of varname logic
         | Pconstant of int
         | Pconstructor of string logic * (t llist) logic

  let show p =
    let rec helper = function
      (* | Ptuple xs -> failwith "tuples should be removed from code base" *)
      | Pany -> "Pany"
      | Pvar s -> sprintf "(Pvar %a)" (fun () -> show_logic_naive) s
      | Pconstant n -> string_of_int n
      | Pconstructor (name, xs) when llist_is_empty_logic xs ->
         sprintf "Pconstructor(\"%a\",[])" sprintf_logic name
      | Pconstructor(name,xs) ->
         sprintf "Pconstructor(\"%a\",%a)" sprintf_logic name sprintf_logic xs
    in
    helper p

  let constructor name xs = Pconstructor (!name, xs)
end

implicit module Show_pat_logic : (SHOW with type t = PatLogic.t) =
  struct
    type t = PatLogic.t
    let show = PatLogic.show
  end

implicit module Show_pat_kind : (SHOW with type t = Pat.kind) =
  struct
    type t = Pat.kind
    let show = Pat.show_kind
  end


implicit module Show_lambda_nologic : (SHOW with type t = MiniLambda_Nologic.lambda)  =
  struct
    type t = MiniLambda_Nologic.lambda
    let show = MiniLambda_Nologic.show
  end

module MiniCompile = struct
  open Pat
  open PatLogic

  let classify_line line kind =
    fresh (h t)
          (line === h%t)
          (conde [ (h === !Pany) &&& (kind === !Kany)
                 ; fresh v        ((h === !(Pvar v)) &&& (kind === !Kvar))
                 ; fresh (n args) ((h === !(Pconstructor (n, args))) &&& (kind === !Kconstructor))
                 ])

  let (_:PatLogic.t llist logic -> Pat.kind logic -> Tester.M.goal) = classify_line
  (* let (_:int) = of_list *)
  (*                     [ of_list [Pvar (!"x"); constructor "[]" llist_nil] *)
  (*                     ; of_list [constructor "[]" llist_nil; Pany] *)
  (*                     ] *)

  open ImplicitPrinters
  open MiniLambda_Nologic
  let rec classify_prefix_helper expected_kind patts top bot =
    fresh (pat_h pat_tl kind2)
          (patts === pat_h % pat_tl)
          (classify_line pat_h kind2)

          (conde [ fresh (top2 bot2)
                         (kind2 === expected_kind)
                         (classify_prefix_helper expected_kind pat_tl top2 bot2)
                         (top === pat_h % top2)
                         (bot === bot2)

                 ; (kind2 =/= expected_kind) &&& (top === llist_nil) &&& (bot === patts)
                 ])
(*  (* In this code it can't find implicit for some reason *)
  let rec classify_prefix_helper expected_kind patts top bot =
    call_fresh @@ fun (right: lambda logic) ->
    fresh (pat_h pat_tl kind2)
          (patts === ( let (_:lambda logic) = right in !(pat_h, right)  ) % pat_tl)
          (classify_line pat_h kind2)

          (conde [ fresh (top2 bot2)
                         (kind2 === expected_kind)
                         (classify_prefix_helper expected_kind pat_tl top2 bot2)
                         (top === !(pat_h,right) % top2)
                         (bot === bot2)

                 ; (kind2 =/= expected_kind) &&& (top === llist_nil) &&& (bot === patts)
                 ])

 *)
(*
  let (_:Pat.kind logic ->
         PatLogic.t llist llist logic ->
         PatLogic.t llist llist logic ->
         _ logic -> _) = classify_prefix_helper *)

  let lambda_hack arg out =
    conde [ (arg === !(Lconst (Vint 1))) &&& (arg === out)
          ; (arg === out)
          ]

  let rec classify_handlers handlers top_patts top_handlers bot_handlers =
    fresh (phd ptl)
          (top_patts === phd % ptl)
          (conde
             [ fresh (x x_lambda_hack)
                     (ptl === llist_nil)
                     (handlers === x % bot_handlers)
                     (lambda_hack x x_lambda_hack)
                     (top_handlers === (!< x) )
             ; fresh (x hs top_h2 bot_h2 x_lambda_hack)
                     (ptl =/= llist_nil)
                     (handlers === x % hs)
                     (lambda_hack x x_lambda_hack)
                     (classify_handlers hs ptl top_h2 bot_h2)
                     (top_handlers === x % top_h2)
                     (bot_handlers === bot_h2)
             ])

  (* let (_:int) = classify_handlers *)

  let classify_prefix patts ans top bot =
    fresh (_1stline t kind)
          (patts === _1stline % t)
          (classify_line _1stline kind)
          (ans === kind)
          (classify_prefix_helper kind patts top bot)

  (* classify single line *)
  let classify_constructor  pats ans =
    fresh (left others)
          (pats === left % others)
          (fresh (name args)
                     (left === !Pconstructor (name, args))
                     (ans === !(name, args, others))
          )
  (* let eval_constructor_group cname  *)
  let classify_constructors matrix ans =
    fresh (phd ptl)
          (matrix === phd % ptl)
          (cond
             [fresh (x)
                    (ptl === llist_nil)


  (* let compile_constructors patts handlers0            *)

  let compile tuple patts handlers ans =
    fresh (new_kind top_pats bot_pats top_handlers bot_handlers)
          (classify_prefix patts new_kind top_pats bot_pats)
          (classify_handlers handlers top_pats top_handlers bot_handlers)
          (ans === llist_nil)

  (* let (_:int) = compile *)
end

let just_a a = a === (embed 5)

open Tester

let _ =
  let open MiniCompile in
  let open PatLogic in
  let (_: PatLogic.t llist logic llist logic) =
                    of_list
                    [ of_list [Pvar (!"x"); constructor "[]" llist_nil]
                    ; of_list [constructor "[]" llist_nil; Pany]
                    ]
  in


  let pats1 = of_list_hack
                      [ of_list_hack [Pvar (!"x"); constructor "[]" llist_nil]
                      ; of_list_hack [constructor "[]" llist_nil; Pany]
                      ]
  in

  (* run1 ~n:1 (REPR classify_prefix); *)
  run1 ~n:1 (REPR (classify_line @@ of_list
                   [constructor "[]" llist_nil; Pany]) );
  run1 ~n:1 (REPR (classify_line @@ of_list
                   [Pvar (!"x"); constructor "[]" llist_nil]) );

  run2 ~n:1 (REPR (classify_prefix_helper !Pat.Kvar
                   !pats1) );
(*
  run3 ~n:1 (REPR
               (classify_prefix @@ embed (
                  of_list_hack
                    [ of_list_hack [Pvar (!"x"); constructor "[]" llist_nil]
                    ; of_list_hack [constructor "[]" llist_nil; Pany]
                    ])) );

 *)
  let open MiniLambda_Nologic in
  run1 ~n:1 (REPR (compile !["lx";"ly"] !pats1
                           (of_list [Lconst (Vint 1); Lconst (Vint 2)]) ) );
  (* run1 ~n:1 (REPR (compile !["lx";"ly"] !pats1 ![] ) ); *)

  ()
