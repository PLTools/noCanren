open Printf
open ImplicitPrinters

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
         | Pvar of string
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
         | Pconstant of int logic
         | Pconstructor of varname logic * (t llist) logic

  let show p =
    let rec helper = function
      (* | Ptuple xs -> failwith "tuples should be removed from code base" *)
      | Pany -> "Pany"
      | Pvar s -> sprintf "(Pvar %a)" (fun () -> show_logic_naive) s
      | Pconstant n -> show_logic_naive n
      | Pconstructor (name, xs) when llist_is_empty_logic xs ->
         sprintf "Pconstructor(\"%a\",[])" sprintf_logic name
      | Pconstructor(name,xs) ->
         sprintf "Pconstructor(\"%a\",%a)" sprintf_logic name sprintf_logic xs
    in
    helper p

  let constructor name xs = Pconstructor (!name, xs)
  let var s = Pvar !s

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

module MiniLambda = struct
  type lambda =
    | Lvar of string
    | Lconst of Value.t logic
    | Lfield of int * string
    | Lcheckconstr of string logic * varname logic
    | Lneq of string logic * Value.t logic
    | Llet of string * lambda * lambda
    | Lifthenelse of lambda logic * lambda logic * lambda logic
    | Lswitchconstr of lambda * (string * lambda) list * lambda option
    | Lstaticraise of int (* * Lambda.lambda list *)
    | Lstaticcatch of lambda logic * (int option) * lambda logic

  open Format

  let rec print ppf root =
    let pr fmt = fprintf ppf fmt in
    let rec helper = function
    | Lvar s   -> fprintf ppf "%s" s
    | Lconst c -> pr "%a" fprintf_logic c
    (* | Lfield (n, ident) -> fprintf ppf "@[get_field@ %d@ %s@ @]" n ident *)
    (* | Lcheckconstr (cname, vname) -> fprintf ppf "@[check_constr@ \"%s\"@ %s@ @]" cname vname *)
    | Lcheckconstr (s,l) -> pr "check_constr \"%a\" %a" fprintf_logic s fprintf_logic l
    | Lneq (varname, val_) ->
       pr "@[%a@ <>@ %a@,@]" fprintf_logic varname fprintf_logic val_
    | Lifthenelse (c,t,b) ->
       pr "if %a then %a else %a" fprintf_logic c fprintf_logic t fprintf_logic b
    | Lstaticraise n -> pr "exit %d" n
    | Lstaticcatch (what, None, handler) ->
       pr "try %a with %a" fprintf_logic what fprintf_logic handler
    | Lstaticcatch (what, Some n, handler) ->
       pr "try %a with %d -> %a" fprintf_logic what n fprintf_logic handler
    | _ -> pr "<not implemented>"
    in
    helper root

  let show lam =
    let b = Buffer.create 10 in
    let fmt = Format.formatter_of_buffer b in
    print fmt lam;
    pp_print_flush fmt ();
    Buffer.contents b

end

implicit module Show_lambda : (SHOW with type t = MiniLambda.lambda)  =
  struct
    type t = MiniLambda.lambda
    let show = MiniLambda.show
  end

implicit module Show_value : (SHOW with type t = Value.t)  =
  struct
    open Format

    type t = Value.t
    let show v =
      let b = Buffer.create 10 in
      let fmt = Format.formatter_of_buffer b in
      Value.print fmt v;
      pp_print_flush fmt ();
      Buffer.contents b

  end

module MiniLambdaHelpers = struct
  open MiniLambda
  let extract_from_const c rez = (c === !(Lconst rez))
  let make_neq ident val_ rez  = (rez === !(Lneq (ident, val_)) )
  let make_lconst c rez = rez === !(Lconst c)
  let make_ifthenelse cond trueb elseb rez = rez === !(Lifthenelse (cond, trueb, elseb))
  let make_lcheckconstr cname varname rez = (rez === !(Lcheckconstr (cname, varname)))
  let make_simple_try what handler rez = (rez === !(Lstaticcatch (what, None, handler)) )
  let make_exit n rez = rez === !(Lstaticraise n)
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
  open MiniLambda
  open MiniLambdaHelpers
  open Value
(*
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
 *)

(*  (* in this code it can't find implicit for some reason *)
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

  type line_t = PatLogic.t llist logic * lambda logic
  type matrix_t = line_t llist
  type vars_tuple = string llist


(*
  (* let (_:int) = !(PatLogic.Pconstructor (!"asdf", llist_nil)) *)
  let lambda_hack (arg: MiniLambda.lambda logic) out =
    conde [ (make_lconst !(Vint 1) arg) &&& (arg === out)
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
    *)
  (* let (_:int) = classify_handlers *)


(*
  let classify_prefix (patts: matrix logic) ans top bot =
    fresh (_1stline right bot kind)
          (patts === !(_1stline,right) % bot)
          (classify_line _1stline kind)
          (ans === kind)
          (classify_prefix_helper kind patts top bot)
    *)
  let get_line_constr_name (line: line_t logic) ans =
    fresh (pats right left others)
          (line === !(pats , right) )
          (pats === left % others)
          (fresh (name args)
                 (left === PatLogic.(embed_explicit show
                                                    (Pconstructor (name, args))) )
                 (ans === (name: string logic))
          )
(*
  (* classify single line *)
  let classify_line_t (line: line_t logic) ans =
    fresh (pats right left others)
          (line === !(pats, right) )
          (pats === left % others)
          (fresh (name args)
                 (left === PatLogic.(embed_explicit show
                                                    (Pconstructor (name, args))) )
                 (ans === !( (name: string logic),
                             (args:(PatLogic.t llist) logic),
                             others))
          )
 *)
  let rec split_constrs_by_name name matrix good bad =
    conde [ (matrix === llist_nil) &&& (good === llist_nil) &&& (bad === llist_nil)
          ; fresh (line1 matrix2 name1 good2 bad2)
                  (matrix === line1 % matrix2)
                  (get_line_constr_name line1 name1)
                  (split_constrs_by_name name matrix2 good2 bad2)
                  (conde [ (name1 === name) &&& (good === line1%good2) &&& (bad === bad2)
                         ; (name1 =/= name) &&& (good === good2) &&& (bad === line1%bad2)
                         ])
          ]

  let pat_is_var p = fresh (name) (p === !(PatLogic.Pvar name))
  let pat_is_any p = p === !PatLogic.Pany
  let rec cut_someth (cond: PatLogic.t logic -> goal) (matrix: matrix_t logic) top
                     (bot: matrix_t logic) =
    first_of
      [ (matrix === llist_nil) &&& (top === llist_nil) &&& (bot === llist_nil)
      ; fresh (bot0 pats right p1 pothers)
              (matrix === !(pats, right) % bot0)
              (pats === p1 % pothers)
              (cond p1)
              (fresh (top1 bot cutted_line)
                     (cut_someth cond bot0 top1 bot)
                     (cutted_line === !(pothers, right))
                     (top === cutted_line % top1))
      ]

  let cut_vars = cut_someth pat_is_var
  let cut_anys = cut_someth pat_is_any
  let cut_vars_or_anys = cut_someth (fun x -> (pat_is_any x) ||| (pat_is_var x))

  (* let eval_constructor_group cname  *)
(*
  let classify_constructors matrix ans =
    fresh (phd ptl)
          (matrix === phd % ptl)
          (cond
             [fresh (x)
                    (ptl === llist_nil)
 *)

  let top_line_of_matrix (matrix: matrix_t logic) (ans: line_t logic) =
    fresh (tl) (matrix === ans % tl)

  let top_line_of_matrix2 (matrix: matrix_t logic) (ans: line_t logic) bot =
    matrix === ans % bot

  let top_line_is_constr (matrix: matrix_t logic) name =
    fresh (h pats right p1 pothers)
          (top_line_of_matrix matrix h)
          (h === !(pats, right))
          (pats === p1 % pothers)
          (fresh (name1 args)
                 (p1 === !(PatLogic.Pconstructor (name1, args)) )
                 (name === name1))

  let top_line_is_not_a_constr (matrix: matrix_t logic) =
    fresh (h pats right p1 pothers)
          (top_line_of_matrix matrix h)
          (h === !(pats, right))
          (pats === p1 % pothers)
          (fresh (name args)
                 (p1 =/= !(PatLogic.Pconstructor (name, args)) ) )

  let destruct_top_constr (line: line_t logic) name cargs (other_pats: line_t logic) =
    fresh (pats right p1 pothers)
          (line === !(pats, right) )
          (pats === p1 % pothers)
          (p1 === !(PatLogic.Pconstructor (name, cargs)) )
          (other_pats === !(pothers,right) )


  let top_line_is_var (matrix: matrix_t logic) dummy =
    fresh (h tl pats right p1 pothers)
          (matrix === h%tl)
          (h === !(pats, right))
          (pats === p1 % pothers)
          (conde
             [ fresh (name1)
                     (p1 === !(PatLogic.Pvar name1))
                     (dummy === !1)
             (* For simplicity we treat vars as _ *)
             ; (p1 === !PatLogic.Pany) &&& (dummy === !1)
             ])



  let top_line_is_constant (matrix: matrix_t logic) =
    fresh (h tl pats right p1 pothers)
          (matrix === h%tl)
          (h === !(pats, right))
          (pats === p1 % pothers)
          (fresh (c)
                 (p1 === !(PatLogic.Pconstant c)) )


  let empty_matrix m = "empty_matrix" <=>
    (m === llist_nil)


  let matrix_zero_width (m: matrix_t logic) ans =
    fresh (h tl right)
          (m === h % tl)
          (h === !(llist_nil, right))
          (ans === !1)

  let top_right_handler (m: matrix_t logic) (handler: lambda logic) =
    fresh (h tl pats)
          (m === h%tl)
          (h === !(pats, handler) )

  let list_tail what ans = fresh (h) (what === h % ans)
  (* let tuple_tail_wtf (what: string llist logic) ans = fresh (h) (what === h % ans) *)

  open MiniLambdaHelpers

  let appendo = Test000.appendo
  let rec list_snoc x xs ans =
    first_of
      [ (xs === llist_nil) &&&  (ans === !< x)
      ; fresh (h tl ans2)
              (xs === h % tl)
              (list_snoc x tl ans2)
              (ans === x % ans2)
      ]
  (* let (_:int) = list_snoc *)
(*
  type constr_collector_arg = (string logic * line_t llist logic)
  type constr_collector = constr_collector_arg llist
  let rec assoc_add (key:string logic) (what: line_t logic)
                    (xs: (string logic * line_t llist logic) llist logic)
                    ans =
    fresh (u k1 v1 tl)
          (xs === u % tl)
          (u  === !(k1, v1) )
          (first_of
             [ fresh (v2)
                     (k1 === key)
                     (list_snoc what v1 v2)
                     (ans === !(k1,v2) % xs)
             ; fresh (ans2)
                     (assoc_add key what tl ans2)
                     (ans === !(k1,v1) % ans2)
             ])
 *)

(*
  let rec cut_named_constrs (m: matrix_t logic)
                            (assoc_list: (string logic * line_t logic) llist logic)
                            bot_ans =
    fresh (line1 bot)
          (top_line_of_matrix m line1 bot)
          (first_of
             [ fresh (name cargs line1')
                     (top_line_is_constr m name)
                     (destruct_top_constr line1 name cargs line1')
                     (assoc_add name !(cargs, line1') assoc_list new_assoc)
                     (cut_named_constrs tl bot new_assoc bot_ans)
             ; (bot_ans === m)
             ])
 *)
  (* check name of constructor on first line and find constructors with the same name.
   * When we get not a constructor -- finish
   *)
  let rec extract_constrs m name top bot =
      first_of
        [ fresh (name')
            (top_line_is_constr m name')
            (first_of
               [ fresh (top1 line1 others)
                       (name === name')
                       (top_line_of_matrix2 m line1 others)
                       (extract_constrs others name top1 bot)
                       (top === line1 % top1)
               (* different names*)
               ; fresh (bot1 line1 others)
                       (name =/= name')
                       (top_line_of_matrix2 m line1 others)
                       (extract_constrs others name top bot1)
                       (* (top_line_is_not_a_constr m) *)
                       (bot === line1 % bot1)
               ])
        ; fresh (__x)
                (* (top_line_is_not_a_constr m) *)
                (top === llist_nil)
                (bot === m)
        ]

  let enlarge_tuple (line: line_t logic) old_tuple new_tuple =
    let rec helper cargs old ans =
      conde
        [ fresh (temp_tuple h tl )
                (cargs === h % tl)
                (helper tl old temp_tuple)
                (ans === !"x" % temp_tuple)
        ; (cargs === llist_nil) &&& (old === ans)
        ]
    in

    fresh (name cargs otherpats)
          (destruct_top_constr line name cargs otherpats)
          (helper cargs old_tuple new_tuple)

  let extract_constrs_helper m name top bot st0 =
    let ans = extract_constrs m name top bot st0 in
    (* Format.(fprintf std_formatter "rework_matrix m=%a name=%a\n -> top=%a bot=%a\n%!" *)
    (*                 fprintf_logic m *)
    (*                 fprintf_logic name *)
    (*                 fprintf_logic top *)
    (*                 fprintf_logic bot); *)
    ans

    (* inlarge matrix by adding arguments of removed constuctors*)
  let rec rework_matrix m mans =
    "rework_matrix" <=>
      conde
        [ (m === llist_nil) &&& (mans === llist_nil)
        ; fresh (line1 bot pats right p1 pothers cname cargs)
                (top_line_of_matrix2 m line1 bot)
                (line1 === !(pats, right) )
                (pats === p1 % pothers)
                (fresh (cname cargs line1' mans')
                   (p1 === !(PatLogic.Pconstructor (cname, cargs)) )
                   (appendo cargs pothers line1')
                   (rework_matrix bot mans')
                   (mans === !(line1', right) % mans')
                )
        ]

  let rework_matrix_helper l r st0 =
    let ans = rework_matrix l r st0 in
    (* Format.(fprintf std_formatter "rework_matrix %a -> %a\n%!" *)
    (*                 fprintf_logic l fprintf_logic r); *)
    ans

  let rec compile_constr_prefix (tuple: string llist logic)
                                (m: matrix_t logic)
                                onfail
                                (ans: lambda logic) =
    let fix_tuple m old_tuple ans =
      fresh (line1)
            (top_line_of_matrix m line1)
            (enlarge_tuple line1 old_tuple ans)
    in

    fresh (top bot name top2 bot2 top_tuple)
          (top_line_is_constr m name)
          (extract_constrs_helper m name top bot2)
          (rework_matrix_helper top top2)
          (fix_tuple m tuple top_tuple)
          (fresh (ans_top ans_bot check_lam)
                 (compile tuple     bot2 onfail  ans_bot)
                 (compile top_tuple top2 onfail ans_top)
                 (make_lcheckconstr name !"x" check_lam)
                 (make_ifthenelse check_lam ans_top ans_bot ans)
          )

  and compile (tuple: string llist logic) (m: matrix_t logic) onfail (ans: lambda logic) =
    (* let () = print_endline @@ *)
    (*            sprintf "compile tuple '%a' m '%a' onfail '%a'\n%!" *)
    (*              sprintf_logic tuple *)
    (*              sprintf_logic m *)
    (*              sprintf_logic onfail *)
    (* in *)
    "compile" <=>
    first_of
      [ (empty_matrix m) &&& (ans === onfail)
      ; fresh (temp)
              (matrix_zero_width m temp)
              (top_right_handler m ans)

      ; fresh (temp top_m bot_m top_tuple onfail2 dummy )
              (top_line_is_var m dummy)
              (cut_vars_or_anys m top_m bot_m)
              (list_tail tuple top_tuple)
              (fresh (what handler exit_from_top)
                     (make_exit 5 exit_from_top)
                     (* (compile tuple     bot_m onfail handler) *)
                     (make_lconst !(Vint 8888) handler)
                     (compile top_tuple top_m exit_from_top what)
                     (make_simple_try what handler ans)
              )
              (* ( tuple === tuple_tail) *)
              (* (compile tuple      bot_m onfail  onfail2) *)
              (* (compile tuple_tail top_m onfail2 ans) *)
              (* (ans === !(Lconst (Vint 7777)) *)
(*
      ; fresh (x)
              (* TODO *)
              (top_line_is_constant m)
              (make_lconst !(Vint 99999) ans)
 *)
      ; fresh (name)
              (top_line_is_constr m name)
              (compile_constr_prefix tuple m onfail ans)
              (* (make_lconst !(Vint 1) ans) *)

      ; (make_lconst !(Vint 1234) ans)
      ]

end

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

  let open MiniLambda in
  let open Value in
  let pats1 = of_list
                [ of_list [Pvar (!"x"); constructor "[]" llist_nil], !(Lconst !(Vint 1))
                ; of_list [constructor "[]" llist_nil;        Pany], !(Lconst !(Vint 2))
                ]
  in

  let pats0 = of_list
                [ of_list [(*Pvar (!"x") ;*) Pany  ], !(Lconst !(Vint 1))
                (* ; of_list [Pany], !(Lconst !(Vint 2)) *)
                ]
  in

  run1 ~n:1 (REPR (classify_line @@ of_list
                   [constructor "[]" llist_nil; Pany]) );
  run1 ~n:1 (REPR (classify_line @@ of_list
                   [Pvar (!"x"); constructor "[]" llist_nil]) );

  let pats2 = of_list
                      [ of_list [constructor "::" llist_nil; Pany], !(Lconst !(Vint 1))
                      ; of_list [constructor "[]" llist_nil; Pany], !(Lconst !(Vint 2))
                      ; of_list [constructor "::" llist_nil; Pany], !(Lconst !(Vint 3))
                      ]
  in
  let pats2_0 = of_list
                      [ of_list [constructor "::" llist_nil], !(Lconst !(Vint 1))
                      ; of_list [constructor "[]" llist_nil], !(Lconst !(Vint 2))
                      ; of_list [constructor "::" llist_nil], !(Lconst !(Vint 3))
                      ]
  in
  let the_empty_matrix : matrix_t logic = llist_nil in
  let z_width_matrix : matrix_t logic =
    of_list [ (llist_nil, !(Lconst !(Vint 3)) )
            ; (llist_nil, !(Lconst !(Vint 4)) )
            ]
  in

  let () = run2 ~n:1 (REPR (split_constrs_by_name (!"::") (pats2)
                                     ) )
  in

  let () = run1 ~n:1 (REPR (top_line_is_constr pats2  ) ) in
  let () = run1 ~n:1 (REPR (compile (of_list [""]) the_empty_matrix !(Lconst !(Vint 666))))
  in
  let () = run1 ~n:1 (REPR (compile (of_list [""]) z_width_matrix  !(Lconst !(Vint 666))))
  in
  let () = run1 ~n:1 (REPR (compile (of_list ["lx";"ly"]) pats1    !(Lconst !(Vint 666))))
  in

  let () = run1 ~n:1 (REPR (compile (of_list ["lx"]) pats0    !(Lconst !(Vint 666))))
  in

  let () = run1 ~n:1 (REPR (compile (of_list ["lx"]) pats2_0    !(Lconst !(Vint 666))))
  in

  let () = run2 ~n:1 (REPR (extract_constrs pats2_0 (!"::") ))
  in

  let example1 = of_list
      [ (* of_list [constructor "[]" llist_nil;              Pany], !(Lconst !(Vint 1)) *)
      (* ; of_list [Pany;        constructor "[]" llist_nil], !(Lconst !(Vint 2)) *)
      (* ; *) of_list [ constructor "::" (of_list [var "x"; var "xs"])
                ; constructor "::" (of_list [var "y"; var "ys"])
                ], !(Lconst !(Vint 3))
      ; of_list [Pany;        constructor "[]" llist_nil], !(Lconst !(Vint 2))
      ]
  in
  let () = run1 ~n:1 (REPR (compile (of_list ["lx"; "ly"]) example1 !(Lconst !(Vint 666))))
  in

  (* let () = run1 ~n:1 (REPR (rework_matrix  example1)) *)
  (* in *)

  ()
