open Printf
(* open MiniKanren *)
open ImplicitPrinters
(* open Tester.M *)

module Value = struct
  type t = Vint of int
         | Vvar of string
         | Vtuple of t list
         | Vconstructor of string * t list

  let is_constructor name = function
    | Vconstructor (n,_) when n = name -> true
    | _ -> false

  let field_exn n : t -> t = function
    | Vint x when n=0 -> Vint x
    | Vvar _
    | Vint _ -> failwith "Bad argument of field_exn"
    | Vconstructor (_,xs) -> List.nth xs n
    | Vtuple xs -> List.nth xs n

  let vpair x y = Vtuple [x;y]
  let vconstructor name xs = Vconstructor (name, xs)

  let print ppf v =
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
      | Vconstructor (name, []) -> fprintf ppf "%s" name
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
type pat = Pany
         | Pvar of varname
         | Pconstant of int
         | Ptuple of pat list
         | Pconstructor of string * pat list
         (* | Por of pat * pat *)


implicit module Show_pat : (SHOW with type t = pat) = struct
    type t = pat
    let rec show = function
      | Pany -> "_"
      | Pvar s -> s
      | Pconstant x -> string_of_int x
      | Ptuple ps -> sprintf "(%s)" @@ String.concat "," @@ List.map show ps
      | Pconstructor (name,xs) -> sprintf "%s %s" name (show (Ptuple xs))
      (* | Por (p1,p2) -> sprintf "%s | %s" (show p1) (show p2) *)
end


let ppair x y = Ptuple [x;y]
let pconstructor name xs = Pconstructor (name,xs)

type source_program = Value.t * (pat * Value.t) list
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
    (* | Lapply of lambda logic * lambda logic llist *)
    (* | Lfunction of function_kind * Ident.t list * lambda *)
    | Llet of ident * lambda * lambda
    (* | Lletrec of (Ident.t * Lambda.lambda) list * Lambda.lambda *)
    (* | Lprim of Lambda.primitive * Lambda.lambda list *)
    (* | Lswitch of lambda * lambda_switch *)
    (* | Lstringswitch of Lambda.lambda * (string * Lambda.lambda) list * *)
    (*                    Lambda.lambda option *)
    | Lstaticraise of int (* * Lambda.lambda list *)
    | Lstaticcatch of lambda * (int (* * Ident.t list *)) * lambda
    (* | Ltrywith of lambda * Ident.t * Lambda.lambda *)
    | Lifthenelse of (lambda * lambda * lambda)
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

  open Format

  let print ppf root =
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
       pr "with@ %d ->@ " n;
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
    in
    helper root;
    fprintf ppf "@."

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
      | Lvar name -> `Ok (env name)
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
          | `Raise m when m=n -> helper env handler
          | `Raise m -> `Raise m
        end
      | Lcheckconstr (constr, varname) ->
         `Ok Value.(if is_constructor constr @@ lookup_env env varname
                    then Vint 1 else Vint 0)
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
      let next_varname =
        let counter = ref 0 in
        fun () -> incr counter; sprintf "x%d" counter.contents
      in
      let root_varname = "rootVar" in
      (*
      let rec match_one_patt env varname patt right elseb =
        match Env.lookup_exn varname env, patt with
        | Vint _, Pany -> right
        | Vint _, Pvar _ -> right
        | Vint n, Pconstant m when m=n -> right
        | Vint _, Pconstant _
        | Vint _, Ptuple _
        | Vint _, Pconstructor _ -> raise Cant_compile

        | Vconstructor (name1,_), Pconstructor(name2,_) when name1<>name2 ->
           raise Cant_compile
        | Vconstructor (_,xs), Pconstructor(_,ys) when List.length xs <> List.length ys ->
           raise Cant_compile
        | Vconstructor (_,xs), Pconstructor (_,ys) ->
           let new_var = next_varname () in
           let new_val = Vtuple xs in
           let new_env = Env.extend new_var new_val env in
           Llet (new_var, Lconst new_val,
                 match_one_patt new_env new_var (Ptuple ys) right elseb)
        (* | Vtuple xs, Ptuple ys -> *)
        (*    List.map2 (fun x y -> (x,y)) xs ys |> *)
        (*    List.mapi (fun n (x,y) -> (i,x,y)) xs ys |> *)
        (*    ListLabels.fold_right *)
        (*      ~init:elseb *)
        (*      ~f:(fun (n,v,pat) acc -> *)
        (*          let new_var = next_varname () in *)
        (*          let new_val = Lfield (n,varname) in *)
        (*          Llet (new_var, new_val, match *)

                (*           match_one_patt env *)
        (*         ) *)
        (* | _ ->  Lconst 0 *)
      in
      *)
      let next_exit_counter =
        let exit_counter = ref 0 in
        fun () -> incr exit_counter; exit_counter.contents
      in
      let rec match_one_patt varname patt right elseb =
        match patt with
        | Pany -> right
        | Pvar newname -> Llet (newname, Lvar varname, right)
        | Pconstant c -> Lifthenelse (Lneq (varname,Vint c),  elseb, right)
        | Pconstructor (name,[]) ->
           lifthenelse (lcheckconstr name varname) right elseb
        | Pconstructor (name,xs) ->
           let exit_code = next_exit_counter () in
           Lstaticcatch(
             Lifthenelse (Lcheckconstr (name, varname),
                          match_one_patt varname (Ptuple xs) right (Lstaticraise exit_code),
                          (Lstaticraise exit_code)),
             exit_code,
             elseb)
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
                             exit_code
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
let () =
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
      ] (Vint 1) ~heading:(fun l -> llet "lx" (Lconst nil) @@ llet "ly" (Lconst nil) l) ;


  ()
