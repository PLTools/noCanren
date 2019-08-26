(* Print all fully qualified names in expressions *)
open Printf
open Asttypes
open Longident
open Typedtree
open Ast_helper
open Tast_mapper

let () = Printexc.record_backtrace true

module Lozov = struct
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
  let () = Location.print f loc in
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

let untyper = Untypeast.default_mapper

let create_id  s = Lident s |> mknoloc |> Exp.ident
let create_pat s = mknoloc s |> Pat.var

let rec lowercase_lident = function
  | Lident s      -> Lident (Util.mangle_construct_name s)
  | Lapply (l, r) -> Lapply (lowercase_lident l, lowercase_lident r)
  | Ldot (t, s)   -> Ldot (lowercase_lident t, s)

let rec is_primary_type (t : Types.type_expr) =
  match t.desc with
  | Tarrow _ -> false
  | Tlink t' -> is_primary_type t'
  | _        -> true


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


let filter_vars vars1 vars2 =
  List.filter (fun v -> List.for_all ((<>) v) vars2) vars1

let mark_type_declaration td =
    match td.typ_kind with
    | Ttype_variant cds -> { td with typ_attributes = [(mknoloc "put_distrib_here", Parsetree.PStr [])] }
    | _                 -> fail_loc td.typ_loc "Incrorrect type declaration"

let mark_constr expr = { expr with
  pexp_attributes = (mknoloc "it_was_constr", Parsetree.PStr []) :: expr.pexp_attributes }

let mark_fo_arg expr = { expr with
  pexp_attributes = (mknoloc "need_CbN", Parsetree.PStr []) :: expr.pexp_attributes }

let create_logic_var name =
  { (create_pat name) with ppat_attributes = [mknoloc "logic", Parsetree.PStr []] }

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
  | Tpat_tuple [l; r] ->
    let args, vars = List.map (fun q -> translate_pat q fresher) [l; r] |> List.split in
    let vars = List.concat vars in
    create_apply (mark_constr [%expr pair]) args, vars
  | _ -> fail_loc pat.pat_loc "Incorrect pattern in pattern matching"

let rec is_disj_pats = function
  | []      -> true
  | x :: xs -> not (List.exists (have_unifier x) xs) && is_disj_pats xs

(*****************************************************************************************************************************)


(*HERE*)

let translate_high tast start_index need_lowercase need_unlazy =

let lowercase_lident x =
  if need_lowercase
  then lowercase_lident x
  else x in

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

let create_fresh_argument_names_by_args (typ : Types.type_expr) =
  match typ.desc with
  | Tarrow (_, _, right_typ, _) -> create_fresh_var_name () :: create_fresh_argument_names_by_type right_typ
  | Tlink typ                   -> create_fresh_argument_names_by_type typ
  | _                           -> [] in

let rec unnest_constuct e =
  match e.exp_desc with
  | Texp_constant c ->
    create_inj (Exp.constant (Untypeast.constant c)), []
  | Texp_construct ({txt = Lident s}, _, []) when s = "true" || s = "false" ->
    create_inj (untyper.expr untyper e), []
  | Texp_tuple [a; b] ->
      let arg1, fv1 = unnest_constuct a in
      let arg2, fv2 = unnest_constuct b in
      create_apply (mark_constr [%expr pair]) [arg1; arg2], fv1 @ fv2
  | Texp_construct (name, _, args) ->
    let new_args, fv = List.map unnest_constuct args |> List.split in
    let fv           = List.concat fv in
    let new_args     = match new_args with [] -> [[%expr ()]] | l  -> l in
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

and translate_ident i =
  match i with
  | "&&"  -> translate_bool_funs false
  | "||"  -> translate_bool_funs true
  | "not" -> translate_not_fun ()
  | "="   -> translate_eq_funs true
  | "<>"  -> translate_eq_funs false
  |  _    -> create_id i

and translate_abstraciton case =

  let rec normalize_abstraction expr acc =
    match expr.exp_desc with
    | Texp_function {arg_label; param; cases=[case]; _} ->
      let Tpat_var (n, _) = case.c_lhs.pat_desc in
      let typ = case.c_lhs.pat_type in
      normalize_abstraction case.c_rhs ((name n, typ) :: acc)
    | _ -> expr, List.rev acc in

  let eta_extension expr =
    let rec get_arg_types (typ : Types.type_expr) =
      match typ.desc with
      | Tarrow (_, l, r, _) -> l :: get_arg_types r
      | Tlink typ           -> get_arg_types typ
      | _                   -> [] in
    let arg_types = get_arg_types expr.exp_type in
    List.map (fun t -> create_fresh_var_name (), t) arg_types in

  let two_or_more_mentions var_name expr =
    let rec two_or_more_mentions expr count =
      let eval_if_need c e = if c <= 1 then two_or_more_mentions e c else c in
      let rec get_pat_vars p =
        match p.pat_desc with
        | Tpat_any
        | Tpat_constant _             -> []
        | Tpat_var (n, _)             -> [name n]
        | Tpat_tuple pats
        | Tpat_construct (_, _, pats) -> List.concat @@ List.map get_pat_vars pats in

      match expr.exp_desc with
      | Texp_constant _ -> count
      | Texp_tuple args
      | Texp_construct (_, _, args) ->
        List.fold_left eval_if_need count args
      | Texp_ident (_, { txt = Longident.Lident name }, _) ->
        if var_name = name then count + 1 else count
      | Texp_function {arg_label; param; cases=[case]; _} ->
        if var_name = get_pat_name case.c_lhs then count else two_or_more_mentions case.c_rhs count
      | Texp_apply (func, args) ->
        let args = List.map (fun (_, Some a) -> a) args in
        List.fold_left eval_if_need count @@ func :: args
      | Texp_ifthenelse (cond, th, Some el) ->
        List.fold_left eval_if_need count [cond; th; el]
      | Texp_let (_, bindings, expr) ->
        let bindings = List.filter (fun b -> var_name <> get_pat_name b.vb_pat) bindings in
        let exprs = expr :: List.map (fun b -> b.vb_expr) bindings in
        List.fold_left eval_if_need count exprs
      | Texp_match (e, cs, _, _) ->
        let cases = List.filter (fun c -> List.for_all ((<>) var_name) @@ get_pat_vars c.c_lhs) cs in
        let exprs = e :: List.map (fun c -> c.c_rhs) cases in
        List.fold_left eval_if_need count exprs in

    two_or_more_mentions expr 0 >= 2 in

  let create_simple_arg var =
    let fresh_n = create_fresh_var_name () in
    create_fun fresh_n [%expr [%e create_id fresh_n] === [%e create_id var]] in

  if need_unlazy then
    let Tpat_var (name0, _) = case.c_lhs.pat_desc in
    let body, real_vars     = normalize_abstraction case.c_rhs [name name0, case.c_lhs.pat_type] in
    let eta_vars            = eta_extension body in
    let translated_body     = translate_expression body in
    let result_var          = create_fresh_var_name () in
    let body_with_eta_args  = create_apply translated_body @@
                              List.map create_id @@
                              List.map fst eta_vars @ [result_var] in
    let bad_vars            = List.filter (fun v -> two_or_more_mentions v body) @@
                              List.map fst @@
                              List.filter (fun (_, t) -> is_primary_type t) @@
                              real_vars @ eta_vars in
    let fresh_vars          = List.map (fun _ -> create_fresh_var_name ()) bad_vars in
    let abstr_body          = List.fold_right create_fun bad_vars body_with_eta_args in
    let body_with_args      = create_apply abstr_body @@
                              List.map create_simple_arg fresh_vars in
    let conjs               = List.map2 (fun a b -> create_apply (create_id a) @@ [create_id b]) bad_vars fresh_vars in
    let full_conj           = create_conj (conjs @ [body_with_args]) in
    let with_fresh          = List.fold_right create_fresh fresh_vars full_conj in
    let first_fun           = create_fun result_var with_fresh in
    let with_eta            = List.fold_right create_fun (List.map fst eta_vars) first_fun in
    List.fold_right create_fun (List.map fst real_vars) with_eta
  else Exp.fun_ Nolabel None (untyper.pat untyper case.c_lhs) (translate_expression case.c_rhs)

and translate_apply f a l =
  create_apply
    (translate_expression f)
    (List.map (function (_, Some e) -> if is_primary_type e.exp_type
                                       then mark_fo_arg @@ translate_expression e
                                       else translate_expression e
                       | _ -> fail_loc l "Incorrect argument") a)

and translate_match loc s cases typ =
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
    let scrutinee = create_apply (translate_expression s) [create_id scrutinee_var] in
    let scrutinee = { scrutinee with pexp_attributes = s.exp_attributes } in
    let conj      = create_conj [scrutinee; disj] in
    let fresh     = create_fresh scrutinee_var conj in
    let with_res  = [%expr fun [%p create_logic_var result_arg] -> [%e fresh]] in
    List.fold_right create_fun high_extra_args with_res

  else fail_loc loc "Pattern matching contains unified patterns"

and translate_let flag bind expr =
  let nvb = Vb.mk (untyper.pat untyper bind.vb_pat) (translate_expression bind.vb_expr) in
  let nvb = if is_primary_type bind.vb_expr.exp_type
            then { nvb with pvb_attributes = [mknoloc "need_CbN", Parsetree.PStr []] }
            else nvb in

  Exp.let_ flag
           [nvb]
           (translate_expression expr)

and translate_expression e =
  match e.exp_desc with
  | Texp_constant _                          -> translate_construct e
  | Texp_construct _                         -> translate_construct e
  | Texp_tuple [l; r]                        -> translate_construct e
  | Texp_ident (_, { txt = Lident name }, _) -> translate_ident name
  | Texp_function {cases = [case]}           -> translate_abstraciton case
  | Texp_apply (f, a)                        -> translate_apply f a e.exp_loc
  | Texp_match (s, cs, _, _)                 -> translate_match e.exp_loc s cs e.exp_type
  | Texp_ifthenelse (cond, th, Some el)      -> translate_if cond th el
  | Texp_let (flag, [bind], expr)            -> translate_let flag bind expr
  | _                                        -> fail_loc e.exp_loc "Incorrect expression" in

let translate_external_value_binding vb =
  let pat  = untyper.pat untyper vb.vb_pat in
  let expr = translate_expression vb.vb_expr in
  let nvb  = Vb.mk pat expr in
  if is_primary_type vb.vb_expr.exp_type
    then { nvb with pvb_attributes = [mknoloc "need_CbN", Parsetree.PStr []] }
    else nvb in

let translate_structure_item i =
  match i.str_desc with
  | Tstr_value (rec_flag, [bind]) ->
      Str.value rec_flag [translate_external_value_binding bind]
  | Tstr_type (rec_flag, decls) ->
    let new_decls = List.map mark_type_declaration decls in
    untyper.structure_item untyper { i with str_desc = Tstr_type (rec_flag, new_decls) }
  | _ -> fail_loc i.str_loc "Incorrect structure item" in

let translate_structure t = List.map translate_structure_item t.str_items in

translate_structure tast

(*****************************************************************************************************************************)

let translate tast start_index need_lowercase need_poly need_false =

let lowercase_lident x =
  if need_lowercase
  then lowercase_lident x
  else x in

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
    if not need_poly && is_primary_type bind.vb_expr.exp_type then
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

    let has_tabled_attr = List.exists (fun a -> (fst a).txt = tabling_attr_name) bind.vb_attributes in

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
    if is_or then  [%expr Bool.oro] else  [%expr Bool.ando]
    (* let a1  = create_fresh_var_name () in
    let a2  = create_fresh_var_name () in
    let q   = create_fresh_var_name () in
    let fst = if is_or then [%expr !!true]  else [%expr !!false] in
    let snd = if is_or then [%expr !!false] else [%expr !!true]  in
    if need_false then
    [%expr fun [%p create_pat a1] [%p create_pat a2] [%p create_pat q] ->
             conde [([%e create_id a1] === [%e fst]) &&& ([%e create_id q] === [%e fst]);
                    ([%e create_id a1] === [%e snd]) &&& ([%e create_id q] === [%e create_id a2])]]
    else if is_or then
    [%expr fun [%p create_pat a1] [%p create_pat a2] [%p create_pat q] ->
             ([%e create_id q] === !!true) &&& (([%e create_id a1] === !!true) ||| ([%e create_id a2] === !!true))]
    else
    [%expr fun [%p create_pat a1] [%p create_pat a2] [%p create_pat q] ->
             ([%e create_id q] === !!true) &&& (([%e create_id a1] === !!true) &&& ([%e create_id a2] === !!true))] *)

  and translate_eq_funs is_eq =
    let a1  = create_fresh_var_name () in
    let a2  = create_fresh_var_name () in
    let q   = create_fresh_var_name () in
    let fst = if is_eq then [%expr !!true]  else [%expr !!false] in
    let snd = if is_eq then [%expr !!false] else [%expr !!true]  in
    if need_false then
    [%expr fun [%p create_pat a1] [%p create_pat a2] [%p create_pat q] ->
             conde [([%e create_id a1] === [%e create_id a2]) &&& ([%e create_id q] === [%e fst]);
                    ([%e create_id a1] =/= [%e create_id a2]) &&& ([%e create_id q] === [%e snd])]]
    else
    [%expr fun [%p create_pat a1] [%p create_pat a2] [%p create_pat q] ->
             ([%e create_id a1] === [%e create_id a2]) &&& ([%e create_id q] === [%e fst])]

  and translate_not_fun () = [%expr Bool.noto]
    (* let a  = create_fresh_var_name () in
    let q  = create_fresh_var_name () in
    [%expr fun [%p create_pat a] [%p create_pat q] ->
             conde [([%e create_id a] === !!true ) &&& ([%e create_id q] === !!false);
                    ([%e create_id a] === !!false) &&& ([%e create_id q] === !!true )]] *)

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

    | Texp_match (e, cs, _, _) -> translate_match let_vars expr.exp_loc e cs expr.exp_type

    | Texp_ifthenelse (cond, th, Some el) -> if need_false
                                             then translate_if let_vars cond th el expr.exp_type
                                             else fail_loc expr.exp_loc "if-then-else expression in not-false mode"

    | Texp_function {cases = [case]} -> translate_abstraciton let_vars case

    | Texp_ident (_, { txt = Lident "="  }, _)  -> translate_eq_funs true
    | Texp_ident (_, { txt = Lident "<>" }, _)  -> if need_false
                                                   then translate_eq_funs false
                                                   else fail_loc expr.exp_loc "Operator '<>' in not-false mode"

    | Texp_ident (_, { txt = Lident "||" }, _)  -> translate_bool_funs true
    | Texp_ident (_, { txt = Lident "&&" }, _)  -> translate_bool_funs false

    | Texp_ident (_, { txt = Lident "not" }, _) -> if need_false
                                                   then translate_not_fun ()
                                                   else fail_loc expr.exp_loc "Operator 'not' in not-false mode"

    | Texp_ident (_, { txt = Lident name }, _) -> translate_ident let_vars name expr.exp_type

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
  List.map (fun n -> Lident n |> mknoloc |> Opn.mk |> Str.open_) packages @ ast

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

  let has_heavy_attr e = List.exists (fun a -> (fst a).txt = "heavy") e.pexp_attributes in

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
    | (x, _) :: xs -> x.Location.txt = attr || has_attr attr xs
    | []           -> false in

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
        let need_tabling = List.map expr_is_logic args in
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

let only_generate ~oldstyle hook_info tast =
  if oldstyle
  then try
    let open Lozov  in
    let need_reduce          = true in
    let need_lower_case      = true in
    let need_normalize       = true in

    let need_poly            = false in
    let need_false           = true in

    let need_high            = false in
    let need_move_unifies_up = true in
    let need_unlazy          = false in
    let need_CbN             = false in
    let subst_only_q         = false in

    let start_index = get_max_index tast in
    let reductor    = beta_reductor start_index subst_only_q in
    (if need_high
      then translate_high tast start_index need_lower_case need_unlazy
      else translate tast start_index need_lower_case need_poly need_false) |>
    add_packages |>
    eval_if_need need_reduce    (reductor.structure reductor) |>
    eval_if_need need_normalize (let mapper = fresh_and_conjs_normalizer need_move_unifies_up in
                                 mapper.structure mapper) |>
    eval_if_need need_CbN call_by_need_creator |>
    PutDistrib.process |>
    print_if Format.std_formatter Clflags.dump_parsetree Printast.implementation |>
    print_if Format.std_formatter Clflags.dump_source Pprintast.structure
  with
    | Lozov.Error e as exc ->
      Lozov.report_error Format.std_formatter e;
      raise exc
  else
  try
    print_endline "new style";
    let reduced_ast = Smart_mapper.process tast in
    reduced_ast |>
    (*PutDistrib.process |>*)
    print_if Format.std_formatter Clflags.dump_parsetree Printast.implementation |>
    print_if Format.std_formatter Clflags.dump_source Pprintast.structure
  with exc ->
    Printexc.print_backtrace stdout;
    raise exc


let main = fun (hook_info : Misc.hook_info) ((tast, coercion) : Typedtree.structure * Typedtree.module_coercion) ->
  let new_ast       = only_generate ~oldstyle:false hook_info tast in

  try
  (*        let new_ast = [] in*)
    let old = ref (!Clflags.print_types) in
    Clflags.print_types := true;
    let (retyped_ast, new_sig, _env) =
      let () = print_endline "retyping generated code" in
      Printexc.print
      (Typemod.type_structure (Compmisc.initial_env()) new_ast)
      Location.none
    in
    Clflags.print_types := !old;
  (*        Printtyped.implementation_with_coercion Format.std_formatter (retyped_ast, coercion);*)
    Printtyp.wrap_printing_env (Compmisc.initial_env()) (fun () ->
      let open Format in
      fprintf std_formatter "%a@."
        Printtyp.signature (Typemod.simplify_signature new_sig));
    (retyped_ast, Tcoerce_none)
  with
    | Lozov.Error e as exc ->
      Lozov.report_error Format.std_formatter e;
      raise exc
(*    | Error e as exc ->
      report_error Format.std_formatter e;
      raise exc*)
    | Env.Error e as exc ->
      Env.report_error Format.std_formatter e;
      Format.printf "\n%!";
      raise exc
    | Typecore.Error (_loc,_env,e) as exc ->
      Typecore.report_error _env Format.std_formatter e;
      Format.printf "\n%!";
      raise exc
    | Typemod.Error (_loc,_env,e) as exc ->
      Typemod.report_error _env Format.std_formatter e;
      Format.printf "\n%!";
      raise exc
    | Typetexp.Error (_loc,_env,e) as exc ->
      Typetexp.report_error _env Format.std_formatter e;
      Format.printf "\n%!";
      raise exc
    | Typemod.Error_forward e as exc ->
      raise exc

(*
(* registering actual translator *)
let () = Typemod.ImplementationHooks.add_hook "ml_to_mk" main
*)
