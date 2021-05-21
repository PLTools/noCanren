(* Print all fully qualified names in expressions *)
open Printf
open Asttypes
open Longident
open Typedtree
open Ast_helper
open Tast_mapper
open Util

let () = Printexc.record_backtrace true

module Transl = struct
open Typedtree
open Ident
open Asttypes
open Lexing
open Location
open Parsetree

(*****************************************************************************************************************************)

let fresh_var_prefix    = "q"
let tabling_attr_name   = "tabled"

let fresh_module_name   = "Fresh"
let fresh_one_name      = "one"
let fresh_two_name      = "two"
let fresh_three_name    = "three"
let fresh_four_name     = "four"
let fresh_five_name     = "five"
let fresh_succ_name     = "succ"

(*****************************************************************************************************************************)

let packages = ["GT"; "OCanren"; "OCanren.Std"]

(*****************************************************************************************************************************)

type error = NotYetSupported of string
exception Error of error

let report_error fmt  = function
| NotYetSupported s -> Format.fprintf fmt "Not supported during relational conversion: %s\n%!" s

let fail_loc loc fmt =
  let b = Buffer.create 100 in
  let f = Format.formatter_of_buffer b in
  let () = Format.fprintf f fmt in
  let () = Format.fprintf f ". " in
  let () = Location.print_loc f loc in
  Format.pp_print_flush f ();
  failwith (Buffer.contents b)

(*****************************************************************************************************************************)

let get_max_index tast =

  let max_index = ref 0 in

  let set_max_index_from_name name =
    let prefix_length = String.length fresh_var_prefix in
    let length = String.length name in
    if length > prefix_length && (String.sub name 0 prefix_length) = fresh_var_prefix then
      let index = try String.sub name prefix_length (length - prefix_length) |> int_of_string with Failure _ -> -1
      in if index > !max_index then max_index := index in

  let expr sub x =
    match x.exp_desc with
    | Texp_ident (path, _, _) -> Path.name path |> set_max_index_from_name; x
    | _                       -> Tast_mapper.default.expr sub x in

  let finder = {Tast_mapper.default with expr} in

  finder.structure finder tast |> ignore; !max_index

(*****************************************************************************************************************************)

let rec fold_right1 f = function
| [h]  -> h
| h::t -> f h (fold_right1 f t)
| []   -> failwith "fold_right1"

let filteri f l =
  let rec filteri l n =
    match l with
    | []               -> []
    | x::xs when f n x -> x :: filteri xs (n+1)
    | _::xs            -> filteri xs (n+1) in
  filteri l 0

let untyper = Untypeast.default_mapper

let create_id  s = Lident s |> mknoloc |> Exp.ident
let create_pat s = mknoloc s |> Pat.var

let rec lowercase_lident = function
  | Lident s      -> Lident (mangle_construct_name s)
  | Lapply (l, r) -> Lapply (lowercase_lident l, lowercase_lident r)
  | Ldot (t, s)   -> Ldot (t, mangle_construct_name s)

let rec is_primary_type (t : Types.type_expr) =
  match t.desc with
  | Tarrow _ -> false
  | Tlink t' -> is_primary_type t'
  | _        -> true

let pat_is_var p =
  match p.pat_desc with
  | Tpat_var (_, _) -> true
  | _               -> false

let get_pat_name p =
  match p.pat_desc with
  | Tpat_var (n, _) -> name n
  | _               -> fail_loc p.pat_loc "Incorrect pattern"


let create_apply f = function
| []   -> f
| args ->
  let args = List.map (fun a -> Nolabel, a) args in
  match f.pexp_desc with
  | Pexp_apply (g, args') -> Exp.apply g (args' @ args)
  | _                     -> Exp.apply f args


let create_apply_to_list f arg_list =
  let new_arg = List.fold_right (fun x acc -> [%expr [%e x] :: [%e acc]]) arg_list [%expr []] in
  create_apply f [new_arg]


let create_conj = function
| []     -> failwith "Conjunction needs one or more arguments"
| [x]    -> x
| [x; y] -> [%expr [%e x] &&& [%e y]]
| l      -> create_apply_to_list [%expr (?&)] l


let create_disj = function
| []     -> failwith "Conjunction needs one or more arguments"
| [x]    -> x
| [x; y] -> [%expr [%e x] ||| [%e y]]
| l      -> create_apply_to_list [%expr conde] l


let create_fun var body =
  [%expr fun [%p create_pat var] -> [%e body]]


let create_fresh var body =
  create_apply [%expr call_fresh] [create_fun var body]


let create_inj expr = [%expr !! [%e expr]]

let rec path2ident = function
  | Path.Pident i      -> Lident (name i)
  | Path.Pdot (l, r)   -> Ldot (path2ident l, r)
  | Path.Papply (l, r) -> Lapply (path2ident l, path2ident r)

let ctor_for_record loc typ =
  let rec get_id (typ : Types.type_expr) =
    match typ.desc with
    | Tlink t           -> get_id t
    | Tconstr (p, _, _) -> begin match path2ident p with
                           | Lident i    -> Lident ("ctor_g" ^ i)
                           | Ldot (l, r) -> Ldot (l, "ctor_g" ^ r)
                           | Lapply _    -> fail_loc loc "What is 'Lapply'?"
                           end
    | _                 -> fail_loc loc "Incorrect type of record" in
  get_id typ |> mknoloc |> Exp.ident


let filter_vars vars1 vars2 =
  List.filter (fun v -> List.for_all ((<>) v) vars2) vars1

let mark_type_declaration td =
    match td.typ_kind with
    | Ttype_variant _
    | Ttype_record  _ -> { td with typ_attributes = [Attr.mk (mknoloc "put_distrib_here") (Parsetree.PStr [])] }
    | _               -> fail_loc td.typ_loc "Incrorrect type declaration"

let mark_constr expr = { expr with
  pexp_attributes = (Attr.mk (mknoloc "it_was_constr") (Parsetree.PStr [])) :: expr.pexp_attributes }

let mark_fo_arg expr = { expr with
  pexp_attributes = (Attr.mk (mknoloc "need_CbN") (Parsetree.PStr [])) :: expr.pexp_attributes }

let is_active_arg pat =
  List.exists (fun a -> a.attr_name.txt = "active") pat.pat_attributes

let create_logic_var name =
  { (create_pat name) with ppat_attributes = [Attr.mk (mknoloc "logic") (Parsetree.PStr [])] }

let rec have_unifier p1 p2 =
  match p1.pat_desc, p2.pat_desc with
  | Tpat_any  , _ | _, Tpat_any
  | Tpat_var _, _ | _, Tpat_var _ -> true
  | Tpat_constant c1, Tpat_constant c2 -> c1 = c2
  | Tpat_tuple t1, Tpat_tuple t2 ->
    List.length t1 = List.length t2 && List.for_all2 have_unifier t1 t2
  | Tpat_construct (_, cd1, a1), Tpat_construct (_, cd2, a2) ->
    cd1.cstr_name = cd2.cstr_name && List.length a1 = List.length a2 && List.for_all2 have_unifier a1 a2
  | _ -> false

let rec translate_pat pat fresher =
  match pat.pat_desc with
  | Tpat_any                                       -> let var = fresher () in create_id var, [var]
  | Tpat_var (v, _)                                -> create_id (name v), [name v]
  | Tpat_constant c                                -> Untypeast.constant c |> Exp.constant |> create_inj, []
  | Tpat_construct ({txt = Lident "true"},  _, []) -> [%expr !!true],  []
  | Tpat_construct ({txt = Lident "false"}, _, []) -> [%expr !!false], []
  | Tpat_construct ({txt = Lident "[]"},    _, []) -> [%expr [%e mark_constr [%expr nil]] ()], []
  | Tpat_construct (id              ,       _, []) -> [%expr [%e lowercase_lident id.txt |> mknoloc |> Exp.ident |> mark_constr] ()], []
  | Tpat_construct ({txt}, _, args)                ->
    let args, vars = List.map (fun q -> translate_pat q fresher) args |> List.split in
    let vars = List.concat vars in
    let constr =
      match txt with
      | Lident "::" -> [%expr (%)]
      | _           -> [%expr [%e lowercase_lident txt |> mknoloc |> Exp.ident]] in
    create_apply (mark_constr constr) args, vars
  | Tpat_tuple l ->
    let args, vars = List.map (fun q -> translate_pat q fresher) l |> List.split in
    let vars = List.concat vars in
    fold_right1 (fun e1 e2 -> create_apply (mark_constr [%expr pair]) [e1; e2]) args, vars
  | Tpat_record (fields, _) ->
    let (_, info, _) = List.hd fields in
    let count = Array.length info.lbl_all in
    let rec translate_record_pat fresher fields index =
      if index == count then [], [] else
      match fields with
      | (_, (i : Types.label_description), _) :: xs when i.lbl_pos > index ->
        let var        = fresher () in
        let pats, vars = translate_record_pat fresher fields (index+1) in
        create_id var :: pats, var :: vars
      | (_, i, p) :: xs ->
        let pat , vars  = translate_pat p fresher in
        let pats, vars' = translate_record_pat fresher xs (index+1) in
        pat :: pats, vars @ vars'
      | [] ->
        let var       = fresher () in
        let pats, vars = translate_record_pat fresher [] (index+1) in
        create_id var :: pats, var :: vars in
    let args, vars = translate_record_pat fresher fields 0 in
    let ctor       = ctor_for_record pat.pat_loc pat.pat_type in
    create_apply ctor args, vars
  | _ -> fail_loc pat.pat_loc "Incorrect pattern in pattern matching"

let rec is_disj_pats = function
  | []      -> true
  | x :: xs -> not (List.exists (have_unifier x) xs) && is_disj_pats xs

(*****************************************************************************************************************************)

let translate_high tast start_index params =

let lowercase_lident x =
  if params.leave_constuctors
  then x
  else lowercase_lident x in

let curr_index = ref start_index in

let create_fresh_var_name () =
  let name = Printf.sprintf "%s%d" fresh_var_prefix !curr_index in
  incr curr_index;
  name in

let rec create_fresh_argument_names_by_type (typ : Types.type_expr) =
  match typ.desc with
  | Tarrow (_, _, right_typ, _) -> create_fresh_var_name () :: create_fresh_argument_names_by_type right_typ
  | Tlink typ                   -> create_fresh_argument_names_by_type typ
  | _                           -> [] in


let rec create_fresh_argument_names_by_args (typ : Types.type_expr) =
  match typ.desc with
  | Tarrow (_, _, right_typ, _) -> create_fresh_var_name () :: create_fresh_argument_names_by_args right_typ
  | Tlink typ                   -> create_fresh_argument_names_by_args typ
  | _                           -> [] in

let rec unnest_constuct e =
  match e.exp_desc with
  | Texp_constant c ->
    create_inj (Exp.constant (Untypeast.constant c)), []
  | Texp_construct ({txt = Lident s}, _, []) when s = "true" || s = "false" ->
    create_inj (untyper.expr untyper e), []
  | Texp_tuple l ->
      let new_args, fv = List.map unnest_constuct l |> List.split in
      let fv           = List.concat fv in
      fold_right1 (fun e1 e2 -> create_apply (mark_constr [%expr pair]) [e1; e2]) new_args, fv
  | Texp_construct (name, _, args) ->
    let new_args, fv = List.map unnest_constuct args |> List.split in
    let fv           = List.concat fv in
    let new_args     = match new_args with [] -> [[%expr ()]] | l -> l in
    let new_name     = match name.txt with
                       | Lident "[]" -> Lident "nil"
                       | Lident "::" -> Lident "%"
                       | txt         -> lowercase_lident txt in
    create_apply (mknoloc new_name |> Exp.ident |> mark_constr) new_args, fv
  | _ ->
    let fr_var = create_fresh_var_name () in
    create_id fr_var, [(fr_var, e)]

and translate_construct expr =
  let constr, binds = unnest_constuct expr in
  let out_var_name  = create_fresh_var_name () in
  let unify_constr  = [%expr [%e create_id out_var_name] === [%e constr]] in
  let conjs         = unify_constr :: List.map (fun (v,e) -> create_apply (translate_expression e) [create_id v]) binds in
  let conj          = create_conj conjs in
  let with_fresh    = List.fold_right create_fresh (List.map fst binds) conj in
  [%expr fun [%p create_logic_var out_var_name] -> [%e with_fresh]]

and translate_bool_funs is_or =
  let a1  = create_fresh_var_name () in
  let a2  = create_fresh_var_name () in
  let b   = create_fresh_var_name () in
  let q   = create_fresh_var_name () in
  let fst = if is_or then [%expr !!true]  else [%expr !!false] in
  let snd = if is_or then [%expr !!false] else [%expr !!true]  in
  [%expr fun [%p create_pat a1] [%p create_pat a2] [%p create_logic_var q] ->
      call_fresh (fun [%p create_pat b] ->
        ([%e create_id a1] [%e create_id b]) &&&
        (conde [
          ([%e create_id b] === [%e fst]) &&& ([%e create_id q] === [%e fst]);
          ([%e create_id b] === [%e snd]) &&& ([%e create_id a2] [%e create_id q])]))]

and translate_eq_funs is_eq =
  let a1  = create_fresh_var_name () in
  let a2  = create_fresh_var_name () in
  let b1  = create_fresh_var_name () in
  let b2  = create_fresh_var_name () in
  let q   = create_fresh_var_name () in
  let fst = if is_eq then [%expr !!true]  else [%expr !!false] in
  let snd = if is_eq then [%expr !!false] else [%expr !!true]  in
  [%expr fun [%p create_pat a1] [%p create_pat a2] [%p create_logic_var q] ->
    call_fresh (fun [%p create_pat b1] ->
    call_fresh (fun [%p create_pat b2] ->
            ([%e create_id a1] [%e create_id b1]) &&&
            ([%e create_id a2] [%e create_id b2]) &&&
            (conde [
              ([%e create_id b1] === [%e create_id b2]) &&& ([%e create_id q] === [%e fst]);
              ([%e create_id b1] =/= [%e create_id b2]) &&& ([%e create_id q] === [%e snd])])))]

and translate_not_fun () =
  let a  = create_fresh_var_name () in
  let b  = create_fresh_var_name () in
  let q  = create_fresh_var_name () in
  [%expr fun [%p create_pat a] [%p create_logic_var q] ->
   call_fresh (fun [%p create_pat b] ->
          ([%e create_id a] [%e create_id b]) &&&
          (conde [
            ([%e create_id b] === !!true ) &&& ([%e create_id q] === !!false);
            ([%e create_id b] === !!false) &&& ([%e create_id q] === !!true )]))]

and translate_if cond th el =
  let b   = create_fresh_var_name () in
  let q   = create_fresh_var_name () in
  [%expr fun [%p create_logic_var q] -> call_fresh (fun [%p create_pat b] ->
      ([%e translate_expression cond] [%e create_id b]) &&& (
        conde [
          ([%e create_id b] === !!true ) &&& ([%e translate_expression th] [%e create_id q]);
          ([%e create_id b] === !!false) &&& ([%e translate_expression el] [%e create_id q])
        ]))]

and translate_ident txt =
  match txt with
  | Lident "&&"  -> translate_bool_funs false
  | Lident "||"  -> translate_bool_funs true
  | Lident "not" -> translate_not_fun ()
  | Lident "="   -> translate_eq_funs true
  | Lident "<>"  -> translate_eq_funs false
  | _            -> mknoloc txt |> Exp.ident

and translate_abstraciton case =

  let rec normalize_abstraction expr acc =
    match expr.exp_desc with
    | Texp_function { cases=[case] } when pat_is_var case.c_lhs ->
      normalize_abstraction case.c_rhs (case.c_lhs :: acc)
    | _ -> expr, List.rev acc in

  let eta_extension expr =
    let rec get_arg_types (typ : Types.type_expr) =
      match typ.desc with
      | Tarrow (_, l, r, _) -> l :: get_arg_types r
      | Tlink typ           -> get_arg_types typ
      | _                   -> [] in
    let arg_types = get_arg_types expr.exp_type in
    List.map (fun t -> create_fresh_var_name (), t) arg_types in

  let two_or_more_mentions tactic var_name expr =
    let rec two_or_more_mentions expr count =
      let eval_if_need c e = if c <= 1 then two_or_more_mentions e c else c in
      let rec get_pat_vars p =
        match p.pat_desc with
        | Tpat_any
        | Tpat_constant _             -> []
        | Tpat_var (n, _)             -> [name n]
        | Tpat_tuple pats
        | Tpat_construct (_, _, pats) -> List.concat @@ List.map get_pat_vars pats
        | Tpat_record (l, _)          -> List.concat @@ List.map (fun (_, _, p) -> get_pat_vars p) l in

      match expr.exp_desc with
      | Texp_constant _ -> count
      | Texp_tuple args
      | Texp_construct (_, _, args) ->
        List.fold_left eval_if_need count args
      | Texp_ident (_, { txt = Longident.Lident name }, _) ->
        if var_name = name then count + 1 else count
      | Texp_ident _ -> count
      | Texp_function { cases } ->
        let cases = List.filter (fun c -> List.for_all ((<>) var_name) @@ get_pat_vars c.c_lhs) cases in
        let exprs = List.map (fun c -> c.c_rhs) cases in
        if tactic = Nondet
          then List.fold_left eval_if_need count @@ exprs
          else List.fold_left max count @@ List.map (eval_if_need count) exprs
      | Texp_apply (func, args) ->
        let args = List.map (fun (_, Some a) -> a) args in
        List.fold_left eval_if_need count @@ func :: args
      | Texp_ifthenelse (cond, th, Some el) ->
        if tactic = Nondet
          then List.fold_left eval_if_need count [cond; th; el]
          else let c1 = eval_if_need count cond in
               max (eval_if_need c1 th) (eval_if_need c1 el)
      | Texp_let (_, bindings, expr) ->
        let bindings = List.filter (fun b -> var_name <> get_pat_name b.vb_pat) bindings in
        let exprs = expr :: List.map (fun b -> b.vb_expr) bindings in
        List.fold_left eval_if_need count exprs
      | Texp_match (e, cs, _) ->
        let cases = List.filter (fun c -> List.for_all ((<>) var_name) @@ get_pat_vars c.c_lhs) cs in
        let exprs = List.map (fun c -> c.c_rhs) cases in
        if tactic = Nondet
          then List.fold_left eval_if_need count @@ e :: exprs
          else let c1 = eval_if_need count e in
               List.fold_left max c1 @@ List.map (eval_if_need c1) exprs
      | Texp_record { fields; extended_expression } ->
        let c' = match extended_expression with
                 | None   -> count
                 | Some e -> eval_if_need count e in
        Array.fold_left (fun c (_, ld) -> match ld with
                                         | Overridden (_, e) -> eval_if_need c e
                                         | _                 -> c
                        ) c' fields
      | Texp_field (e, _, _) -> eval_if_need count e

in
    two_or_more_mentions expr 0 >= 2 in

  let need_to_activate p e =
    is_primary_type p.pat_type && (
      is_active_arg p ||
        params.high_order_paprams.activate_tactic <> Off &&
        two_or_more_mentions params.high_order_paprams.activate_tactic (get_pat_name p) e
      ) in

  let create_simple_arg var =
    let fresh_n = create_fresh_var_name () in
    create_fun fresh_n [%expr [%e create_id fresh_n] === [%e create_id var]] in

  let body, real_vars     = normalize_abstraction case.c_rhs [case.c_lhs] in
  let eta_vars            = eta_extension body in
  let translated_body     = translate_expression body in
  let result_var          = create_fresh_var_name () in
  let body_with_eta_args  = create_apply translated_body @@
                            List.map create_id @@
                            List.map fst eta_vars @ [result_var] in
  let active_vars         = List.map get_pat_name @@
                            List.filter (fun p -> need_to_activate p body) real_vars in
  let fresh_vars          = List.map (fun _ -> create_fresh_var_name ()) active_vars in
  let abstr_body          = List.fold_right create_fun active_vars body_with_eta_args in
  let body_with_args      = create_apply abstr_body @@
                            List.map create_simple_arg fresh_vars in
  let conjs               = List.map2 (fun a b -> create_apply (create_id a) @@ [create_id b]) active_vars fresh_vars in
  let full_conj           = create_conj (conjs @ [body_with_args]) in
  let with_fresh          = List.fold_right create_fresh fresh_vars full_conj in
  let first_fun           = create_fun result_var with_fresh in
  let with_eta            = List.fold_right create_fun (List.map fst eta_vars) first_fun in
  List.fold_right create_fun (List.map get_pat_name real_vars) with_eta

and translate_apply f a l =
  create_apply
    (translate_expression f)
    (List.map (function (_, Some e) -> if is_primary_type e.exp_type
                                       then mark_fo_arg @@ translate_expression e
                                       else translate_expression e
                       | _ -> fail_loc l "Incorrect argument") a)

and translate_match_without_scrutinee loc cases (typ : Types.type_expr) =
  match typ.desc with
    | Tarrow (_, _, r, _) ->
      let new_scrutinee    = create_fresh_var_name () in
      let translated_match = translate_match loc (create_id new_scrutinee) [] cases r in
      create_fun new_scrutinee translated_match
    | Tlink typ ->  translate_match_without_scrutinee loc cases typ
    | _  -> fail_loc loc "Incorrect type for 'function'"

and translate_match_with_scrutinee loc s cases typ =
  translate_match loc (translate_expression s) s.exp_attributes cases typ

and translate_match loc translated_scrutinee attrs cases typ =
  if cases |> List.map (fun c -> c.c_lhs) |> is_disj_pats then
    let high_extra_args = create_fresh_argument_names_by_args typ in
    let result_arg = create_fresh_var_name () in
    let extra_args = high_extra_args @ [result_arg] in
    let scrutinee_var = create_fresh_var_name () in

    let create_subst v =
      let abs_v = create_fresh_var_name () in
      let unify = [%expr [%e create_id v] === [%e create_id abs_v]] in
      create_fun abs_v unify in

    let translate_case case =
      let pat, vars  = translate_pat case.c_lhs create_fresh_var_name in
      let unify      = [%expr [%e create_id scrutinee_var] === [%e pat]] in
      let body       = create_apply (translate_expression case.c_rhs) (List.map create_id extra_args) in
      let abst_body  = List.fold_right create_fun vars body in
      let subst      = List.map create_subst vars in
      let total_body = create_apply abst_body subst in
      let conj       = create_conj [unify; total_body] in
      List.fold_right create_fresh vars conj in

    let new_cases = List.map translate_case cases in
    let disj      = create_disj new_cases in
    let scrutinee = create_apply translated_scrutinee [create_id scrutinee_var] in
    let scrutinee = { scrutinee with pexp_attributes = attrs } in
    let conj      = create_conj [scrutinee; disj] in
    let fresh     = create_fresh scrutinee_var conj in
    let with_res  = [%expr fun [%p create_logic_var result_arg] -> [%e fresh]] in
    List.fold_right create_fun high_extra_args with_res

  else fail_loc loc "Pattern matching contains unified patterns"

and translate_bind bind =
  let rec is_func_type (t : Types.type_expr) =
    match t.desc with
    | Tarrow _ -> true
    | Tlink t' -> is_func_type t'
    | _        -> false in

  let rec has_func_arg (t : Types.type_expr) =
    match t.desc with
    | Tarrow (_,f,s,_) -> is_func_type f || has_func_arg s
    | Tlink t'         -> has_func_arg t'
    | _                -> false in

  let rec get_tabling_rank (typ : Types.type_expr) =
    match typ.desc with
    | Tarrow (_, _, right_typ, _) -> create_apply [%expr Tabling.succ] [get_tabling_rank right_typ]
    | Tlink typ                   -> get_tabling_rank typ
    | _                           -> [%expr Tabling.one] in

  let body            = bind.vb_expr in
  let new_body        = translate_expression body in
  let typ             = body.exp_type in
  let has_tabled_attr = List.exists (fun a -> a.attr_name.txt = tabling_attr_name) bind.vb_attributes in
  let tabled_body     =
    if not has_tabled_attr || has_func_arg typ then new_body else
      let name                  = get_pat_name bind.vb_pat in
      let unrec_body            = create_fun name new_body in
      let recfunc_argument_name = create_fresh_var_name () in
      let recfunc_argument      = create_id recfunc_argument_name in

      let argument_names1       = create_fresh_argument_names_by_type typ in
      let arguments1            = List.map create_id argument_names1 in
      let res_arg_name_1        = create_fresh_var_name () in
      let rec_arg_1             = create_id res_arg_name_1 in

      let argument_names2       = create_fresh_argument_names_by_type typ in
      let arguments2            = List.map create_id argument_names2 in

      let recfunc_with_args     = create_apply recfunc_argument (List.append arguments2 [rec_arg_1]) in
      let conjuncts1            = List.map2 (fun q1 q2 -> create_apply q1 [q2]) arguments1 arguments2 in
      let conjs_and_recfunc     = create_conj @@ List.append conjuncts1 [recfunc_with_args] in
      let freshing_and_recfunc  = List.fold_right create_fresh argument_names2 conjs_and_recfunc in
      let lambdas_and_recfunc   = List.fold_right create_fun (List.append argument_names1 [res_arg_name_1]) freshing_and_recfunc in

      let argument_names3       = create_fresh_argument_names_by_type typ in
      let arguments3            = List.map create_id argument_names3 in

      let argument_names4       = create_fresh_argument_names_by_type typ in
      let arguments4            = List.map create_id argument_names4 in
      let unified_vars1         = List.map2 (fun a b -> [%expr [%e a] === [%e b]]) arguments3 arguments4 in
      let lambda_vars1          = List.map2 create_fun argument_names3 unified_vars1 in

      let new_nody              = create_apply unrec_body (lambdas_and_recfunc :: lambda_vars1) in
      let lambda_new_body       = List.fold_right create_fun (recfunc_argument_name :: argument_names4) new_nody in

      let abling_rank           = get_tabling_rank typ in
      let tabled_body           = create_apply [%expr Tabling.tabledrec] [abling_rank ; lambda_new_body] in

      let argument_names5       = create_fresh_argument_names_by_type typ in
      let arguments5            = List.map create_id argument_names5 in
      let res_arg_name_5        = create_fresh_var_name () in
      let rec_arg_5             = create_id res_arg_name_5 in

      let argument_names6       = create_fresh_argument_names_by_type typ in
      let arguments6            = List.map create_id argument_names6 in

      let tabled_body_with_args = create_apply tabled_body (List.append arguments6 [rec_arg_5]) in

      let conjuncts2            = List.map2 (fun q1 q2 -> create_apply q1 [q2]) arguments5 arguments6 in
      let conjs_and_tabled      = create_conj (List.append conjuncts2 [tabled_body_with_args]) in
      let freshing_and_tabled   = List.fold_right create_fresh argument_names6 conjs_and_tabled in
      let lambdas_and_tabled    = List.fold_right create_fun (List.append argument_names5 [res_arg_name_5]) freshing_and_tabled in
      lambdas_and_tabled in

  let nvb = Vb.mk (untyper.pat untyper bind.vb_pat) tabled_body in
  if is_primary_type bind.vb_expr.exp_type
    then { nvb with pvb_attributes = [Attr.mk (mknoloc "need_CbN") (Parsetree.PStr [])] }
    else nvb

and translate_let flag bind expr =
  let nvb = translate_bind bind in
  Exp.let_ flag
           [nvb]
           (translate_expression expr)

and translate_new_record fields typ loc =
  let fields  = Array.to_list fields in
  let ctor    = ctor_for_record loc typ in
  let mvar    = create_fresh_var_name () in
  let vars    = List.map (fun _ -> create_fresh_var_name ()) fields in
  let exprs   = List.map (fun (_, Overridden (_, expr)) -> translate_expression expr) fields in
  let calls   = List.map2 (fun e v -> create_apply e [create_id v]) exprs vars in
  let uni     = [%expr [%e create_id mvar] === [%e create_apply ctor (List.map create_id vars)]] in
  let conj    = create_conj (uni :: calls) in
  let with_fr = List.fold_right create_fresh vars conj in
  create_fun mvar with_fr

and translate_extended_record expr fields typ loc =
  let fields   = Array.to_list fields in
  let ctor     = ctor_for_record loc typ in
  let mvar     = create_fresh_var_name () in
  let vars     = List.map (fun _ -> create_fresh_var_name ()) fields in
  let bas_call = create_apply (translate_expression expr) [create_apply ctor (List.map create_id vars)] in
  let exprs    = fields |> List.map (function
                              | (_, Overridden (_, e)) -> Some (translate_expression e, create_fresh_var_name ())
                              | _                      -> None) in
  let calls    = exprs |> List.filter_map (function
                              | Some (e, v) -> Some (create_apply e [create_id v])
                              | None        -> None) in
  let real_vs  = List.map2 (fun e v ->
                            match (e, v) with
                              | Some (_, v), _ -> v
                              | None,        v -> v) exprs vars in
  let add_vs   = exprs |> List.filter_map (function
                              | Some (e, v) -> Some v
                              | None        -> None) in
  let uni      = [%expr [%e create_id mvar] === [%e create_apply ctor (List.map create_id real_vs)]] in
  let conj     = create_conj (uni :: bas_call :: calls) in
  let with_fr  = List.fold_right create_fresh (vars @ add_vs) conj in
  create_fun mvar with_fr

and translate_field expr field (field_info : Types.label_description) loc =
  let ctor         = ctor_for_record loc expr.exp_type in
  let mvar         = create_fresh_var_name () in
  let fields       = Array.to_list field_info.lbl_all in
  let index        = field_info.lbl_pos in
  let all_vars     = fields |> List.mapi (fun i _ ->
                           if i == index then mvar else create_fresh_var_name ()) in
  let without_mvar = all_vars |> filteri (fun i _ -> i <> index) in
  let arg          = create_apply ctor (List.map create_id all_vars) in
  let call         = create_apply (translate_expression expr) [arg] in
  let with_fr      = List.fold_right create_fresh without_mvar call in
  create_fun mvar with_fr

and translate_expression e =
  match e.exp_desc with
  | Texp_constant _                                      -> translate_construct e
  | Texp_construct _                                     -> translate_construct e
  | Texp_tuple _                                         -> translate_construct e
  | Texp_ident (_, { txt }, _)                           -> translate_ident txt
  | Texp_function {cases = [c]} when pat_is_var c.c_lhs  -> translate_abstraciton c
  | Texp_function { cases }                              -> translate_match_without_scrutinee e.exp_loc cases e.exp_type
  | Texp_apply (f, a)                                    -> translate_apply f a e.exp_loc
  | Texp_match (s, cs, _)                                -> translate_match_with_scrutinee e.exp_loc s cs e.exp_type
  | Texp_ifthenelse (cond, th, Some el)                  -> translate_if cond th el
  | Texp_let (flag, [bind], expr)                        -> translate_let flag bind expr
  | Texp_record { fields; extended_expression = None }   -> translate_new_record fields e.exp_type e.exp_loc
  | Texp_record { fields; extended_expression = Some a } -> translate_extended_record a fields e.exp_type e.exp_loc
  | Texp_field  (expr, field, field_info)                -> translate_field expr field field_info e.exp_loc
  | _                                                   -> fail_loc e.exp_loc "Incorrect expression" in

let translate_structure_item i =
  match i.str_desc with
  | Tstr_value (rec_flag, [bind]) ->
      Str.value rec_flag [translate_bind bind]
  | Tstr_type (rec_flag, decls) ->
    let new_decls = List.map mark_type_declaration decls in
    untyper.structure_item untyper { i with str_desc = Tstr_type (rec_flag, new_decls) }
  | Tstr_open _ -> untyper.structure_item untyper i
  | _ -> fail_loc i.str_loc "Incorrect structure item" in

let translate_structure t = List.map translate_structure_item t.str_items in

translate_structure tast

(*****************************************************************************************************************************)
(*****************************************************************************************************************************)
(*****************************************************************************************************************************)
(*****************************************************************************************************************************)
(*****************************************************************************************************************************)

let translate tast start_index params =

let lowercase_lident x =
  if params.leave_constuctors
  then x
  else lowercase_lident x in

let curr_index = ref start_index in

let create_fresh_var_name () =
  let name = Printf.sprintf "%s%d" fresh_var_prefix !curr_index in
  incr curr_index;
  name in

let rec create_fresh_argument_names_by_type (typ : Types.type_expr) =
  match typ.desc with
  | Tarrow (_, _, right_typ, _) -> create_fresh_var_name () :: create_fresh_argument_names_by_type right_typ
  | Tlink typ                   -> create_fresh_argument_names_by_type typ
  | _                           -> [create_fresh_var_name ()] in

  (*************************************************)

  let rec unnest_expr let_vars expr =
    match expr.exp_desc with
    | Texp_ident (_, { txt = Longident.Lident name }, _) when List.for_all ((<>) name) let_vars -> untyper.expr untyper expr, []
    | Texp_constant c -> create_inj (Exp.constant (Untypeast.constant c)), []
    | Texp_construct ({txt = Lident s}, _, []) when s = "true" || s = "false" -> create_inj (untyper.expr untyper expr), []
    | Texp_tuple [a; b] ->
      let new_args, fv = List.map (unnest_expr let_vars) [a; b] |> List.split in
      let fv           = List.concat fv in
      create_apply (mark_constr [%expr pair]) new_args, fv

    | Texp_construct (name, _, args) ->
      let new_args, fv = List.map (unnest_expr let_vars) args |> List.split in
      let fv           = List.concat fv in

      let new_args     = match new_args with
                         | [] -> [[%expr ()]]
                         | l  -> l in

      let new_name     = match name.txt with
                         | Lident "[]" -> Lident "nil"
                         | Lident "::" -> Lident "%"
                         | txt         -> lowercase_lident txt in

      create_apply (mknoloc new_name |> Exp.ident |> mark_constr) new_args, fv

    | _ when is_primary_type expr.exp_type ->
      let fr_var = create_fresh_var_name () in
      create_id fr_var, [(fr_var, expr)]
    | _ -> translate_expression let_vars expr, []


  and translate_construct let_vars expr =
    let constr, binds = unnest_expr let_vars expr in
    let out_var_name  = create_fresh_var_name () in
    let unify_constr  = [%expr [%e create_id out_var_name] === [%e constr]] in
    let conjs         = unify_constr :: List.map (fun (v,e) -> create_apply (translate_expression let_vars e) [create_id v]) binds in
    let conj          = create_conj conjs in
    let with_fresh    = List.fold_right create_fresh (List.map fst binds) conj in
    create_fun out_var_name with_fresh


  and translate_ident let_vars name typ =
    if is_primary_type typ && List.for_all ((<>) name) let_vars
      then let var = create_fresh_var_name () in
           [%expr fun [%p create_pat var] -> [%e create_id name] === [%e create_id var]]
      else create_id name


  and translate_abstraciton let_vars case =
    let let_vars = filter_vars let_vars [get_pat_name case.c_lhs] in
    Exp.fun_ Nolabel None (untyper.pat untyper case.c_lhs) (translate_expression let_vars case.c_rhs)


  and normalize_apply expr =
    match expr.exp_desc with
    | Texp_apply (f, args_r) ->
      let expr', args_l = normalize_apply f in
      expr', args_l @ List.map (function | (_, Some x) -> x | _ -> fail_loc expr.exp_loc "Incorrect argument") args_r
    | _ -> expr, []


  and translate_apply let_vars expr =
    let f, args = normalize_apply expr in
    let new_args, binds = List.map (unnest_expr let_vars) args |> List.split in
    let binds = List.concat binds in
    if List.length binds = 0
      then create_apply (translate_expression let_vars f) new_args
      else let eta_vars   = create_fresh_argument_names_by_type expr.exp_type in
           let eta_call   = create_apply (translate_expression let_vars f) (new_args @ List.map create_id eta_vars) in
           let conjs      = List.map (fun (v,e) -> create_apply (translate_expression let_vars e) [create_id v]) binds @ [eta_call] in
           let full_conj  = create_conj conjs in
           let with_fresh = List.fold_right create_fresh (List.map fst binds) full_conj in
           List.fold_right create_fun eta_vars with_fresh


   and eta_form_for_let let_part let_vars expr =
     let transl_expr = translate_expression let_vars expr in
     if is_primary_type expr.exp_type
     then let new_var = create_fresh_var_name () in
      [%expr fun [%p create_pat new_var] -> [%e create_apply transl_expr [create_id new_var] |> let_part]]
     else let_part transl_expr


  and translate_nonrec_let let_vars bind expr =
    if not params.unnesting_params.polymorphism_supported && is_primary_type bind.vb_expr.exp_type then
      let name       = get_pat_name bind.vb_pat in
      let conj1      = create_apply (translate_expression let_vars bind.vb_expr) [create_id name] in
      let args       = create_fresh_argument_names_by_type expr.exp_type in
      let conj2      = create_apply (translate_expression let_vars expr) (List.map create_id args) in
      let both       = create_conj [conj1; conj2] in
      let with_fresh = create_fresh name both in
      List.fold_right create_fun args with_fresh
    else
    let new_let_vars =
      if is_primary_type bind.vb_expr.exp_type
      then get_pat_name bind.vb_pat :: let_vars
      else let_vars in
    let let_part = Exp.let_ Nonrecursive [Vb.mk (untyper.pat untyper bind.vb_pat) (translate_expression let_vars bind.vb_expr)] in
    eta_form_for_let let_part new_let_vars expr


  and translate_rec_let let_vars bind expr =
    let rec is_func_type (t : Types.type_expr) =
      match t.desc with
      | Tarrow _ -> true
      | Tlink t' -> is_func_type t'
      | _        -> false in

    let rec has_func_arg (t : Types.type_expr) =
      match t.desc with
      | Tarrow (_,f,s,_) -> is_func_type f || has_func_arg s
      | Tlink t'         -> has_func_arg t'
      | _                -> false in

    let rec get_tabling_rank (typ : Types.type_expr) =
      match typ.desc with
      | Tarrow (_, _, right_typ, _) -> create_apply [%expr Tabling.succ] [get_tabling_rank right_typ]
      | Tlink typ                   -> get_tabling_rank typ
      | _                           -> [%expr Tabling.one] in

    let body = translate_expression let_vars bind.vb_expr in
    let typ  = bind.vb_expr.exp_type in

    let has_tabled_attr = List.exists (fun a -> a.attr_name.txt = tabling_attr_name) bind.vb_attributes in

    if not has_tabled_attr
    then let let_part = Exp.let_ Recursive [Vb.mk (untyper.pat untyper bind.vb_pat) body] in
         eta_form_for_let let_part let_vars expr
    else if has_func_arg typ
         then fail_loc bind.vb_loc "Tabled function has functional argument"
         else let name = get_pat_name bind.vb_pat in
              let abst = create_fun name body in
              let rank = get_tabling_rank typ in
              let appl = create_apply [%expr Tabling.tabledrec] [rank; abst] in
              let let_part = Exp.let_ Nonrecursive [Vb.mk (untyper.pat untyper bind.vb_pat) appl] in
              eta_form_for_let let_part let_vars expr

  and translate_let let_vars flag bind expr =
    match flag with
    | Recursive    -> translate_rec_let    let_vars bind expr
    | Nonrecursive -> translate_nonrec_let let_vars bind expr


  and translate_match let_vars loc expr cases typ =
    let args = create_fresh_argument_names_by_type typ in

    let scrutinee_is_var =
      match expr.exp_desc with
      | Texp_ident (_, { txt = Longident.Lident name }, _) -> not @@ List.mem name let_vars
      | _                                                  -> false in

    let scrutinee_var =
      match expr.exp_desc with
      | Texp_ident (_, { txt = Longident.Lident name }, _) -> if scrutinee_is_var then name else create_fresh_var_name ()
      | Texp_ident _                                       -> fail_loc expr.exp_loc "Incorrect variable"
      | _                                                  -> create_fresh_var_name () in

    let rec rename var1 var2 pat =
      match pat.pexp_desc with
      | Pexp_ident { txt = Lident name } -> if name = var1 then create_id var2 else pat
      | Pexp_apply (f, args) -> List.map snd args |> List.map (rename var1 var2) |> create_apply f
      | _ -> pat in

    let translate_case case =
      let pat, vars  = translate_pat case.c_lhs create_fresh_var_name in
      let is_overlap = List.mem scrutinee_var vars in
      let new_var    = if is_overlap then create_fresh_var_name () else "" in
      let pat        = if is_overlap then rename scrutinee_var new_var pat else pat in
      let vars       = if is_overlap then List.map (fun n -> if n = scrutinee_var then new_var else n) vars else vars in


      let unify      = [%expr [%e create_id scrutinee_var] === [%e pat]] in
      let body       = create_apply (translate_expression (filter_vars let_vars vars) case.c_rhs) (List.map create_id args) in
      let body       = if is_overlap then create_apply (create_fun scrutinee_var body) [create_id new_var] else body in
      let conj       = create_conj [unify; body] in
      List.fold_right create_fresh vars conj in

    if cases |> List.map (fun c -> c.c_lhs) |> is_disj_pats then

      let new_cases  = List.map translate_case cases in
      let disj       = create_disj new_cases in
      let with_fresh =
        if scrutinee_is_var then disj
        else create_conj [create_apply (translate_expression let_vars expr) [create_id scrutinee_var]; disj] |> create_fresh scrutinee_var in

      List.fold_right create_fun args with_fresh

    else fail_loc loc "Pattern matching contains unified patterns"

  and translate_bool_funs is_or =
    if params.unnesting_params.use_standart_bool_relations then
      if is_or then [%expr Bool.oro] else [%expr Bool.ando]
      else
        let a1  = create_fresh_var_name () in
        let a2  = create_fresh_var_name () in
        let q   = create_fresh_var_name () in
        let fst = if is_or then [%expr !!true]  else [%expr !!false] in
        let snd = if is_or then [%expr !!false] else [%expr !!true]  in
        if params.unnesting_params.remove_false then
          if is_or then
            [%expr fun [%p create_pat a1] [%p create_pat a2] [%p create_pat q] ->
                      ([%e create_id q] === !!true) &&& (([%e create_id a1] === !!true) ||| ([%e create_id a2] === !!true))]
          else
            [%expr fun [%p create_pat a1] [%p create_pat a2] [%p create_pat q] ->
                      ([%e create_id q] === !!true) &&& (([%e create_id a1] === !!true) &&& ([%e create_id a2] === !!true))]
        else [%expr fun [%p create_pat a1] [%p create_pat a2] [%p create_pat q] ->
                 conde [([%e create_id a1] === [%e fst]) &&& ([%e create_id q] === [%e fst]);
                        ([%e create_id a1] === [%e snd]) &&& ([%e create_id q] === [%e create_id a2])]]

  and translate_eq_funs is_eq =
    let a1  = create_fresh_var_name () in
    let a2  = create_fresh_var_name () in
    let q   = create_fresh_var_name () in
    let fst = if is_eq then [%expr !!true]  else [%expr !!false] in
    let snd = if is_eq then [%expr !!false] else [%expr !!true]  in
    if params.unnesting_params.remove_false then
    [%expr fun [%p create_pat a1] [%p create_pat a2] [%p create_pat q] ->
             ([%e create_id a1] === [%e create_id a2]) &&& ([%e create_id q] === [%e fst])]
    else
    [%expr fun [%p create_pat a1] [%p create_pat a2] [%p create_pat q] ->
             conde [([%e create_id a1] === [%e create_id a2]) &&& ([%e create_id q] === [%e fst]);
                    ([%e create_id a1] =/= [%e create_id a2]) &&& ([%e create_id q] === [%e snd])]]

  and translate_not_fun () =
    if params.unnesting_params.use_standart_bool_relations then [%expr Bool.noto]
    else
      let a  = create_fresh_var_name () in
      let q  = create_fresh_var_name () in
      [%expr fun [%p create_pat a] [%p create_pat q] ->
               conde [([%e create_id a] === !!true ) &&& ([%e create_id q] === !!false);
                      ([%e create_id a] === !!false) &&& ([%e create_id q] === !!true )]]

  and translate_if let_vars cond th el typ =
    let args = create_fresh_argument_names_by_type typ in

  let cond_is_var =
    match cond.exp_desc with
    | Texp_ident _ -> true
    | _            -> false in

  let cond_var =
    match cond.exp_desc with
    | Texp_ident (_, { txt = Longident.Lident name }, _) -> name
    | Texp_ident _                                       -> fail_loc cond.exp_loc "Incorrect variable"
    | _                                                  -> create_fresh_var_name () in

  let th = create_apply (translate_expression let_vars th) (List.map create_id args) in
  let el = create_apply (translate_expression let_vars el) (List.map create_id args) in

  let body = [%expr conde [([%e create_id cond_var] === !!true ) &&& [%e th];
                           ([%e create_id cond_var] === !!false) &&& [%e el]]]
  in

  let with_fresh =
    if cond_is_var then body
    else [%expr call_fresh (fun [%p create_pat cond_var] -> ([%e translate_expression let_vars cond] [%e create_id cond_var]) &&& [%e body])] in

    List.fold_right create_fun args with_fresh

  and translate_expression let_vars expr =
    match expr.exp_desc with
    | Texp_constant _          -> translate_construct let_vars expr
    | Texp_construct _         -> translate_construct let_vars expr

    | Texp_tuple [l; r]        -> translate_construct let_vars expr

    | Texp_apply _             -> translate_apply let_vars expr

    | Texp_match (e, cs, _)    -> translate_match let_vars expr.exp_loc e cs expr.exp_type

    | Texp_ifthenelse (cond, th, Some el) -> if params.unnesting_params.remove_false
                                             then fail_loc expr.exp_loc "if-then-else expression in not-false mode"
                                             else translate_if let_vars cond th el expr.exp_type

    | Texp_function {cases = [case]} -> translate_abstraciton let_vars case

    | Texp_ident (_, { txt = Lident "="  }, _)  -> translate_eq_funs true
    | Texp_ident (_, { txt = Lident "<>" }, _)  -> if params.unnesting_params.remove_false
                                                   then fail_loc expr.exp_loc "Operator '<>' in not-false mode"
                                                   else translate_eq_funs false

    | Texp_ident (_, { txt = Lident "||" }, _)  -> translate_bool_funs true
    | Texp_ident (_, { txt = Lident "&&" }, _)  -> translate_bool_funs false

    | Texp_ident (_, { txt = Lident "not" }, _) -> if params.unnesting_params.remove_false
                                                   then fail_loc expr.exp_loc "Operator 'not' in not-false mode"
                                                   else translate_not_fun ()

    | Texp_ident (_, { txt = Lident name }, _)  -> translate_ident let_vars name expr.exp_type
    | Texp_ident (_, { txt },               _)  -> mknoloc txt |> Exp.ident

    | Texp_let (flag, [bind], expr) -> translate_let let_vars flag bind expr

    | Texp_let _ -> fail_loc expr.exp_loc "Operator LET ... AND isn't supported" (*TODO support LET ... AND*)
    | _ -> fail_loc expr.exp_loc "Incorrect expression"
  in

  let translate_external_value_binding let_vars vb =
    let pat  = untyper.pat untyper vb.vb_pat in
    let expr = translate_expression let_vars vb.vb_expr in
    Vb.mk pat expr in


  let translate_structure_item let_vars stri =
    match stri.str_desc with
    | Tstr_value (rec_flag, [bind]) ->
        Str.value rec_flag [translate_external_value_binding let_vars bind]
    | Tstr_type (rec_flag, decls) ->
      let new_decls = List.map mark_type_declaration decls in
      untyper.structure_item untyper { stri with str_desc = Tstr_type (rec_flag, new_decls) }
    | Tstr_open _ -> untyper.structure_item untyper stri
    | _ -> fail_loc stri.str_loc "Incorrect structure item" in


  let translate_structure str =
    let rec translate_items let_vars = function
    | []    -> []
    | x::xs ->
      let new_let_vars =
        match x.str_desc with
        | Tstr_value (Nonrecursive, [{vb_expr; vb_pat = {pat_desc = Tpat_var (var, _)}}]) when is_primary_type vb_expr.exp_type -> name var :: let_vars
        | _                                                                                                                     -> let_vars in
      translate_structure_item let_vars x :: translate_items new_let_vars xs in

    translate_items [] str.str_items in


  translate_structure tast

(*****************************************************************************************************************************)

let add_packages ast =
  List.map (fun n -> Lident n |> mknoloc |> Mod.ident |> Opn.mk |> Str.open_) packages @ ast

(*****************************************************************************************************************************)

let beta_reductor minimal_index only_q =

  let need_subst name arg = not only_q ||
    let arg_is_var =
      match arg.pexp_desc with
      | Pexp_ident _ -> true
      | _            -> false in

    let prefix_length = String.length fresh_var_prefix in
    let length        = String.length name in

    let index = if length > prefix_length && (String.sub name 0 prefix_length) = fresh_var_prefix
                then try String.sub name prefix_length (length - prefix_length) |> int_of_string
                     with Failure _ -> -1
                else -1 in

    index >= minimal_index || arg_is_var in

  let name_from_pat pat =
    match pat.ppat_desc with
    | Ppat_var loc -> loc.txt
    | _            -> fail_loc pat.ppat_loc "Incorrect pattern in beta reduction" in

  let rec substitute' expr var subst =
    match expr.pexp_desc with
    | Pexp_ident {txt = Lident name} -> if name = var then subst else expr
    | Pexp_fun (_, _, pat, body) ->
      let name = name_from_pat pat in
      if name = var then expr else [%expr fun [%p pat] -> [%e substitute body var subst]]
    | Pexp_apply (func, args) ->
      List.map snd args |>
      List.map (fun a -> substitute a var subst) |>
      create_apply (substitute func var subst)
    | Pexp_let (flag, vbs, expr) ->
      let is_rec       = flag = Recursive in
      let var_in_binds = List.map (fun vb -> name_from_pat vb.pvb_pat) vbs |> List.exists ((=) var) in

      let subst_in_bind bind =
        if is_rec && var_in_binds || not is_rec && var = (name_from_pat bind.pvb_pat)
        then bind
        else { bind with pvb_expr = substitute bind.pvb_expr var subst } in

      let new_vbs = List.map subst_in_bind vbs in
      Exp.let_ flag new_vbs (if var_in_binds then expr else substitute expr var subst)

    | Pexp_construct (name, Some expr) -> Some (substitute expr var subst) |> Exp.construct name
    | Pexp_tuple exprs -> List.map (fun e -> substitute e var subst) exprs |> Exp.tuple
    | _ -> expr

  and substitute expr var subst =
    let res = substitute' expr var subst in
    { res with pexp_attributes = expr.pexp_attributes } in

  let rec beta_reduction' expr args =
    match expr.pexp_desc with
    | Pexp_apply (func, args') ->
      let old_args = List.map snd args' in
      let new_args = List.map (fun a -> beta_reduction a []) old_args in
      List.append new_args args |> beta_reduction func

    | Pexp_fun (_, _, pat, body) ->
      let var = match pat.ppat_desc with
                | Ppat_var v -> v.txt
                | _          -> fail_loc pat.ppat_loc "Incorrect arg name in beta reduction" in
      begin match args with
        | arg::args' when need_subst var arg -> beta_reduction (substitute body var arg) args'
        | _ -> create_apply ([%expr fun [%p pat] -> [%e beta_reduction body []]]) args
      end
    | Pexp_let (flag, vbs, expr) ->
      let new_vbs  = List.map (fun v -> { v with pvb_expr = beta_reduction v.pvb_expr [] }) vbs in
      let new_expr = beta_reduction expr args in
      Exp.let_ flag  new_vbs new_expr

    | Pexp_construct (name, Some expr) -> Some (beta_reduction expr []) |> Exp.construct name
    | Pexp_tuple args -> List.map (fun a -> beta_reduction a []) args |> Exp.tuple
    | _ -> create_apply expr args

  and beta_reduction expr args =
    let res = beta_reduction' expr args in
    { res with pexp_attributes = expr.pexp_attributes } in

  let expr _ x = beta_reduction x [] in
  { Ast_mapper.default_mapper with expr }

(*****************************************************************************************************************************)

let fresh_and_conjs_normalizer need_move_unifies_up =

  let has_heavy_attr e = List.exists (fun a -> a.attr_name.txt = "heavy") e.pexp_attributes in

  let rec split_conjs = function
  | []      -> [], [], []
  | c :: cs ->
    let unifies, conjs, heavies = split_conjs cs in
    match c.pexp_desc with
    | Pexp_apply ({pexp_desc = Pexp_ident {txt = Lident "==="}}, _) ->
      c :: unifies, conjs, heavies
    | _ when has_heavy_attr c -> unifies, conjs, c :: heavies
    | _ -> unifies, c :: conjs, heavies in


  let rec get_conjs_and_vars expr =
    match expr.pexp_desc with
    | Pexp_apply ({pexp_desc = Pexp_ident {txt = Lident "call_fresh"}},
                  [_, {pexp_desc = Pexp_fun (_, _, {ppat_desc = Ppat_var {txt}}, body)}]) ->
      let conjs, vars = get_conjs_and_vars body in
      conjs, txt :: vars

    | Pexp_apply ({pexp_desc = Pexp_ident {txt = Lident "&&&"}}, [_, a; _, b]) ->
      let conjs1, vars1 = get_conjs_and_vars a in
      let conjs2, vars2 = get_conjs_and_vars b in
      conjs1 @ conjs2, vars1 @ vars2

    | Pexp_apply ({pexp_desc = Pexp_ident {txt = Lident "?&"}}, [_, args]) ->
      let rec get_conjs_and_vars_from_list args =
        match args.pexp_desc with
        | Pexp_construct ({txt = Lident "[]"}, _) -> [], []
        | Pexp_construct ({txt = Lident "::"}, Some {pexp_desc = Pexp_tuple [hd;tl]}) ->
          let conjs1, vars1 = get_conjs_and_vars hd in
          let conjs2, vars2 = get_conjs_and_vars_from_list tl in
          conjs1 @ conjs2, vars1 @ vars2
        | _ -> fail_loc args.pexp_loc "Bad args in fresh var upper" in
      get_conjs_and_vars_from_list args

    | _ -> [expr], [] in

  let rec normalizer sub expr =
    let conjs, vars = get_conjs_and_vars expr in
    let conjs = List.map (Ast_mapper.default_mapper.expr sub) conjs in

    let conjs =
      if need_move_unifies_up then
        let unifies, conjs, heavies = split_conjs conjs in
        unifies @ conjs @ heavies
      else conjs in

    let vars_as_apply = function
      | x::xs -> create_apply (create_id x) (List.map create_id xs)
      | _     -> failwith "Incorrect variable count" in

    let vars_arg = function
      | [v] -> Exp.tuple [create_id v]
      | _   -> vars_as_apply vars in

    if List.length vars > 0
    then create_apply [%expr fresh] (vars_arg vars :: conjs)
    else create_conj conjs in

    { Ast_mapper.default_mapper with expr = normalizer }

(*****************************************************************************************************************************)

type var_type = First
              | High

(* extra argument for tabling *)
let ea4t = "ea4t"

let call_by_need_creator tree =

  let rec has_attr attr = function
    | x :: xs -> x.attr_name.Location.txt = attr || has_attr attr xs
    | []      -> false in

  let has_attr_for_expr attr expr = has_attr attr expr.pexp_attributes in

  let is_ctor expr = has_attr_for_expr "it_was_constr" expr in

  let ignored_idents = ["|||"; "&&&"; "conde"; "==="; "=/="; "!!"] in

  let rec uniq = function
    | []      -> []
    | x :: xs -> x :: uniq (List.filter ((<>) x) xs) in

  let remove_vars rem l = List.filter (fun x -> List.for_all ((<>) x) rem) l in

  let get_name_from_pat pat =
    match pat.ppat_desc with
    | Ppat_var x -> x.txt
    | _          -> failwith "Incorrect pattern in call-by-need creator." in

  let is_ident expr =
    match expr.pexp_desc with
    | Pexp_ident _ -> true
    | _            -> false in

  let is_fresh_ident expr =
    match expr.pexp_desc with
    | Pexp_ident {txt = Lident "fresh"} -> true
    | _                                 -> false in

  let get_name_from_expr expr =
    match expr.pexp_desc with
    | Pexp_ident { txt = Lident n } -> n
    | _ -> failwith "Incorrect variable in expression." in

  let convert_fresh_apply_to_vars expr =
    match expr.pexp_desc with
    | Pexp_apply (f, args) ->
      begin match f.pexp_desc with
      | Pexp_ident { txt = Lident fst_var } ->
        fst_var :: List.map (fun (_, e) -> get_name_from_expr e) args
      | _ -> failwith "Incorrect the first variable in fresh-expression."
      end
    | Pexp_tuple [e] -> [get_name_from_expr e]
    | _ -> failwith "Incorrect list of variables in fresh-expression." in

  let rec fv expr =
    match expr.pexp_desc with
    | Pexp_ident {txt = Lident name} ->
      if is_ctor expr || List.exists ((=) name) ignored_idents
      then []
      else [name]

    | Pexp_construct (_, Some expr) -> fv expr

    | Pexp_tuple args -> uniq @@ List.concat @@ List.map fv args

    | Pexp_let (rec_flag, vbs, body) ->
      let exprs     = List.map (fun vb -> vb.pvb_expr) vbs in
      let bind_vars = List.map (fun vb -> get_name_from_pat vb.pvb_pat) vbs in
      let fvs       = List.map fv exprs in
      let fvs       = if rec_flag = Recursive
                      then List.map (remove_vars bind_vars) fvs
                      else fvs in
      let body_fv   = remove_vars bind_vars @@ fv body in
      uniq (body_fv @ List.concat fvs)

    | Pexp_fun (_, _, pat, body) ->
      let bind_var = get_name_from_pat pat in
      let fvs      = fv body in
      List.filter ((<>) bind_var) fvs

    | Pexp_apply (f, args) ->
      let args = List.map snd args in
      if is_fresh_ident f
      then
        let vars :: bodies = args in
        let vars = convert_fresh_apply_to_vars vars in
        uniq @@ remove_vars vars @@ List.concat @@ List.map fv bodies
      else uniq (fv f @ List.concat (List.map fv args))

    | _ -> [] in


  let rec lookup name = function
    | [] -> failwith @@ Printf.sprintf "Environment has no var \"%s\"." name
    | x :: xs ->
      match x with
      | (n, _) -> if n = name then x else lookup name xs in

  let rec split_vars env = function
    | []      -> [], []
    | x :: xs ->
      let f, h = split_vars env xs in
      match lookup x env with
      | n, First -> n :: f, h
      | n, High  -> f, n :: h in

  let vb_is_logic vb = has_attr "need_CbN" vb.pvb_attributes in

  let expr_is_logic e = has_attr "need_CbN" e.pexp_attributes in

  let is_unify_expr e =
    match e.pexp_desc with
    | Pexp_fun (_, _, _, body) ->
      begin match body.pexp_desc with
      | Pexp_apply (f, [_; _]) ->
        begin match f.pexp_desc with
        | Pexp_ident { txt = Lident n } -> n = "==="
        | _ -> false
        end
      | _ -> false
      end
    | _ -> false in

  let var_is_logic pat = has_attr "logic" pat.ppat_attributes in

  let rec update_expr env e =
    match e.pexp_desc with
    | Pexp_ident {txt = Lident name} ->
      if is_ctor e || snd (lookup name env) = First then e
      else [%expr snd [%e e]]
    | Pexp_apply (f, args) ->
      if is_ctor f then e else
      let name = get_name_from_expr f in
      if name = "===" || name = "=/=" || name = "!!" then e else
      let args = List.map snd args in
      if name = "&&&" || name = "|||" || name = "conde" then
        create_apply f @@ List.map (update_expr env) args
      else if name = "fresh" then
        let v::conjs = args in
        let vars = convert_fresh_apply_to_vars v in
        let new_env = List.map (fun x -> (x, First)) vars @ env in
        let new_conjs = List.map (update_expr new_env) conjs in
        create_apply f (v::new_conjs)
      else
        let need_tabling = List.map (fun a -> expr_is_logic a && not @@ is_unify_expr a) args in
        let new_args = List.map2 (update_arg env) args need_tabling in
        create_apply [%expr snd [%e f]] new_args
    | Pexp_fun (_, _, pat, body) ->
      let typ = if var_is_logic pat then First else High in
      let new_body = update_expr ((get_name_from_pat pat, typ) :: env) body in
      [%expr fun [%p pat] -> [%e new_body]]
    | Pexp_let (rf, binds, expr) ->
      let new_vbs, new_env = update_vbs env rf binds in
      Exp.let_ rf new_vbs @@ update_expr new_env expr
    | Pexp_construct (n, Some e) -> Exp.construct n @@ Some (update_expr env e)
    | Pexp_construct (_, None) -> e
    | Pexp_tuple args -> Exp.tuple @@ List.map (update_expr env) args
    | _ -> e

  and update_arg ?(rec_name = None) env arg need_tabling =
    if is_ident arg then arg else
    let vars = fv arg in
    let vars =
      match rec_name with
      | None -> vars
      | Some n -> List.filter ((<>) n) vars in
    let f, h = split_vars env vars in
    let h_part =
      match h with
      | []      -> [%expr []]
      | h :: hs ->
        List.fold_left
          (fun acc h -> [%expr fst [%e create_id h] @ [%e acc]])
          [%expr fst [%e create_id h]] hs in
    let full = List.fold_left
               (fun acc f -> [%expr Obj.magic [%e create_id f] :: [%e acc]])
               h_part f in
    let new_arg = update_expr env arg in
    let new_arg =
      if need_tabling then
        let abst  = create_fun ea4t new_arg in
        let table = [%expr Tabling.tabled Tabling.two [%e abst]] in
        create_apply table [[%expr List.list [%e create_id ea4t]]]
      else new_arg in
    let tuple = Exp.tuple [create_id ea4t; new_arg] in
    [%expr let [%p create_pat ea4t] : (int, int) injected GT.list = [%e full] in [%e tuple]]

  and update_vb is_rec env vb =
    let rec_name = if is_rec then Some (get_name_from_pat vb.pvb_pat) else None in
    { vb with pvb_expr = update_arg ~rec_name:rec_name env vb.pvb_expr (vb_is_logic vb) }

  and update_vbs env rf vbs =
    let names   = List.map (fun vb -> get_name_from_pat vb.pvb_pat) vbs in
    let new_env = List.map (fun n -> (n, High)) names @ env in
    let is_rec  = rf = Recursive in
    let vb_env  = if is_rec then new_env else env in
    let new_vbs = List.map (update_vb is_rec vb_env) vbs in
    new_vbs, new_env in

  let rec structure env = function
    | [] -> []
    | { pstr_desc = Pstr_value (rf, vbs) } :: xs ->
      let new_vbs, new_env = update_vbs env rf vbs in
      Str.value rf new_vbs :: structure new_env xs
    | x :: xs -> x :: structure env xs
    in

  structure [] tree

end

let attrs_remover =
  let expr          sub expr = Ast_mapper.default_mapper.expr          sub { expr with pexp_attributes = [] } in
  let value_binding sub vb   = Ast_mapper.default_mapper.value_binding sub { vb   with pvb_attributes  = [] } in
  let pat           sub pat  = Ast_mapper.default_mapper.pat           sub { pat  with ppat_attributes = [] } in
  { Ast_mapper.default_mapper with expr; value_binding; pat }

let print_if ppf flag printer arg =
  if !flag then Format.fprintf ppf "%a@." printer arg;
  arg

let eval_if_need flag f =
  if flag then f else fun x -> x

let only_generate tast params =
  try
    let open Transl in
    let start_index = get_max_index tast in
    let reductor    = beta_reductor start_index params.subst_only_util_vars in
    (if params.unnesting_mode then translate else translate_high) tast start_index params |>
    add_packages |>
    eval_if_need params.beta_reduction
                 (reductor.structure reductor) |>
    eval_if_need params.normalization
                 (let mapper = fresh_and_conjs_normalizer params.move_unifications in mapper.structure mapper) |>
    eval_if_need params.high_order_paprams.use_call_by_need call_by_need_creator |>
    Put_distrib.process params.useGT |>
    print_if Format.std_formatter Clflags.dump_parsetree Printast.implementation |>
    print_if Format.std_formatter Clflags.dump_source Pprintast.structure
  with
    | Transl.Error e as exc ->
      Transl.report_error Format.std_formatter e;
      raise exc
