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

let tabling_module_name = "Tabling"
let tabling_one_name    = "one"
let tabling_succ_name   = "succ"
let tabling_rec_name    = "tabledrec"

let tabling_attr_name   = "tabled"

let fresh_module_name   = "Fresh"
let fresh_one_name      = "one"
let fresh_two_name      = "two"
let fresh_three_name    = "three"
let fresh_four_name     = "four"
let fresh_five_name     = "five"
let fresh_succ_name     = "succ"

(*****************************************************************************************************************************)

let packages = ["MiniKanren"; "MiniKanrenStd"]

(*****************************************************************************************************************************)

type error = NotYetSupported of string
exception Error of error

let report_error fmt  = function
| NotYetSupported s -> Format.fprintf fmt "Not supported during relational conversion: %s\n%!" s

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
  | Tpat_var (name, _) -> name.name
  | _                  -> failwith "Incorrect pattern"


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

(*****************************************************************************************************************************)

let translate tast start_index =

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

  let rec unnest_expr expr =
    match expr.exp_desc with
    | Texp_ident _ -> untyper.expr untyper expr, []
    | Texp_constant c -> create_inj (Exp.constant (Untypeast.constant c)), []
    | Texp_construct ({txt = Lident s}, _, []) when s = "true" || s = "false" -> create_inj (untyper.expr untyper expr), []

    | Texp_construct (name, _, args) ->
      let new_args, fv = List.map unnest_expr args |> List.split in
      let fv           = List.concat fv in

      let new_args     = match new_args with
                         | [] -> [[%expr ()]]
                         | l  -> l in

      let new_name     = match name.txt with
                         | Lident "[]" -> Lident "nil"
                         | Lident "::" -> Lident "%"
                         | txt         -> lowercase_lident txt in

      create_apply (Exp.ident (mknoloc new_name)) new_args, fv

    | _ when is_primary_type expr.exp_type ->
      let fr_var = create_fresh_var_name () in
      create_id fr_var, [(fr_var, expr)]
    | _ -> translate_expression expr, []


  and translate_construct expr =
    let constr, binds = unnest_expr expr in
    let out_var_name  = create_fresh_var_name () in
    let unify_constr  = [%expr [%e create_id out_var_name] === [%e constr]] in
    let conjs         = unify_constr :: List.map (fun (v,e) -> create_apply (translate_expression e) [create_id v]) binds in
    let conj          = create_conj conjs in
    let with_fresh    = List.fold_right create_fresh (List.map fst binds) conj in
    create_fun out_var_name with_fresh


  and translate_ident name typ =
    if is_primary_type typ
      then let var = create_fresh_var_name () in
           [%expr fun [%p create_pat var] -> [%e create_id name] === [%e create_id var]]
      else create_id name


  and translate_abstraciton case =
    Exp.fun_ Nolabel None (untyper.pat untyper case.c_lhs) (translate_expression case.c_rhs)


  and normalize_apply expr =
    match expr.exp_desc with
    | Texp_apply (f, args_r) ->
      let expr', args_l = normalize_apply f in
      expr', args_l @ List.map (function | (_, Some x) -> x | _ -> failwith "Incorrect argument") args_r
    | _ -> expr, []


  and translate_apply expr =
    let f, args = normalize_apply expr in
    let new_args, binds = List.map unnest_expr args |> List.split in
    let binds = List.concat binds in
    if List.length binds = 0
      then create_apply (translate_expression f) new_args
      else let eta_vars   = create_fresh_argument_names_by_type expr.exp_type in
           let eta_call   = create_apply (translate_expression f) (new_args @ List.map create_id eta_vars) in
           let conjs      = List.map (fun (v,e) -> create_apply (translate_expression e) [create_id v]) binds @ [eta_call] in
           let full_conj  = create_conj conjs in
           let with_fresh = List.fold_right create_fresh (List.map fst binds) full_conj in
           List.fold_right create_fun eta_vars with_fresh


  and translate_let flag bind expr =
    if flag = Recursive || not (is_primary_type bind.vb_expr.exp_type)
      then Exp.let_ flag [Vb.mk (untyper.pat untyper bind.vb_pat) (translate_expression bind.vb_expr)] (translate_expression expr)
      else let name       = get_pat_name bind.vb_pat in
           let conj1      = create_apply (translate_expression bind.vb_expr) [create_id name] in
           let args       = create_fresh_argument_names_by_type expr.exp_type in
           let conj2      = create_apply (translate_expression expr) (List.map create_id args) in
           let both       = create_conj [conj1; conj2] in
           let with_fresh = create_fresh name both in
           List.fold_right create_fun args with_fresh


  and translate_match expr cases typ =
    let args = create_fresh_argument_names_by_type typ in

    let scrutinee_is_var =
      match expr.exp_desc with
      | Texp_ident _ -> true
      | _            -> false in

    let scrutinee_var =
      match expr.exp_desc with
      | Texp_ident (_, { txt = Longident.Lident name }, _) -> name
      | Texp_ident _                                       -> failwith "Incorrect variable"
      | _                                                  -> create_fresh_var_name () in

    let rec translate_pat pat =
      match pat.pat_desc with
      | Tpat_any                                       -> let var = create_fresh_var_name () in create_id var, [var]
      | Tpat_var (v, _)                                -> create_id v.name, [v.name]
      | Tpat_constant c                                -> Untypeast.constant c |> Exp.constant |> create_inj, []
      | Tpat_construct ({txt = Lident "true"},  _, []) -> [%expr !!true],  []
      | Tpat_construct ({txt = Lident "false"}, _, []) -> [%expr !!false], []
      | Tpat_construct ({txt = Lident "[]"},    _, []) -> [%expr nil ()],  []
      | Tpat_construct (id              ,       _, []) -> [%expr [%e lowercase_lident id.txt |> mknoloc |> Exp.ident] ()], []
      | Tpat_construct ({txt = Lident "::"}, _, [a;b]) ->
        let e1, v1 = translate_pat a in
        let e2, v2 = translate_pat b in
        [%expr [%e e1] % [%e e2]], v1 @ v2
      | Tpat_construct (ident, _, args) ->
        let args, vars = List.map translate_pat args |> List.split in
        let vars = List.concat vars in
        create_apply (lowercase_lident ident.txt |> mknoloc |> Exp.ident) args, vars
      | _ -> failwith "Incorrect pattern in pattern matching" in

    let rec rename var1 var2 pat =
      match pat.pexp_desc with
      | Pexp_ident { txt = Lident name } -> if name = var1 then create_id var2 else pat
      | Pexp_apply (f, args) -> List.map snd args |> List.map (rename var1 var2) |> create_apply f
      | _ -> pat in

    let translate_case case =
      let pat, vars  = translate_pat case.c_lhs in

      let is_overlap = List.exists ((=) scrutinee_var) vars in
      let new_var    = if is_overlap then create_fresh_var_name () else "" in
      let pat        = if is_overlap then rename scrutinee_var new_var pat else pat in
      let vars       = if is_overlap then List.map (fun n -> if n = scrutinee_var then new_var else n) vars else vars in

      let unify      = [%expr [%e create_id scrutinee_var] === [%e pat]] in
      let body       = create_apply (translate_expression case.c_rhs) (List.map create_id args) in
      let body       = if is_overlap then create_apply (create_fun scrutinee_var body) [create_id new_var] else body in
      let conj       = create_conj [unify; body] in
      List.fold_right create_fresh vars conj in

    let new_cases  = List.map translate_case cases in
    let disj       = create_disj new_cases in
    let with_fresh = if scrutinee_is_var
                     then disj
                     else create_conj [create_apply (translate_expression expr) [create_id scrutinee_var]; disj]
                       |> create_fresh scrutinee_var in

    List.fold_right create_fun args with_fresh


  and translate_bool_funs is_or =
    let a1  = create_fresh_var_name () in
    let a2  = create_fresh_var_name () in
    let q   = create_fresh_var_name () in
    let fst = if is_or then [%expr !!true]  else [%expr !!false] in
    let snd = if is_or then [%expr !!false] else [%expr !!true]  in
    [%expr fun [%p create_pat a1] [%p create_pat a2] [%p create_pat q] ->
             conde [([%e create_id a1] === [%e fst]) &&& ([%e create_id q] === [%e fst]);
                    ([%e create_id a1] === [%e snd]) &&& ([%e create_id q] === [%e create_id a2])]]


  and translate_eq_funs is_eq =
    let a1  = create_fresh_var_name () in
    let a2  = create_fresh_var_name () in
    let q   = create_fresh_var_name () in
    let fst = if is_eq then [%expr !!true]  else [%expr !!false] in
    let snd = if is_eq then [%expr !!false] else [%expr !!true]  in
    [%expr fun [%p create_pat a1] [%p create_pat a2] [%p create_pat q] ->
             conde [([%e create_id a1] === [%e create_id a2]) &&& ([%e create_id q] === [%e fst]);
                    ([%e create_id a1] =/= [%e create_id a2]) &&& ([%e create_id q] === [%e snd])]]


  and translate_not_fun () =
    let a  = create_fresh_var_name () in
    let q  = create_fresh_var_name () in
    [%expr fun [%p create_pat a] [%p create_pat q] ->
             conde [([%e create_id a] === !!true ) &&& ([%e create_id q] === !!false);
                    ([%e create_id a] === !!false) &&& ([%e create_id q] === !!true )]]


  and translaet_if cond th el typ =
  let args = create_fresh_argument_names_by_type typ in

  let cond_is_var =
    match cond.exp_desc with
    | Texp_ident _ -> true
    | _            -> false in

  let cond_var =
    match cond.exp_desc with
    | Texp_ident (_, { txt = Longident.Lident name }, _) -> name
    | Texp_ident _                                       -> failwith "Incorrect variable"
    | _                                                  -> create_fresh_var_name () in

  let th = create_apply (translate_expression th) (List.map create_id args) in
  let el = create_apply (translate_expression el) (List.map create_id args) in

  let body = [%expr conde [([%e create_id cond_var] === !!true ) &&& [%e th];
                           ([%e create_id cond_var] === !!false) &&& [%e el]]] in

  let with_fresh =
    if cond_is_var then body
    else [%expr call_fresh (fun [%p create_pat cond_var] -> ([%e translate_expression cond] [%e create_id cond_var]) &&& [%e body])] in

    List.fold_right create_fun args with_fresh


  and translate_expression expr =
    match expr.exp_desc with
    | Texp_constant _          -> translate_construct expr
    | Texp_construct _         -> translate_construct expr

    | Texp_apply _             -> translate_apply expr

    | Texp_match (e, cs, _, _) -> translate_match e cs expr.exp_type

    | Texp_ifthenelse (cond, th, Some el) -> translaet_if cond th el expr.exp_type

    | Texp_function {cases = [case]} -> translate_abstraciton case

    | Texp_ident (_, { txt = Lident "="  }, _)  -> translate_eq_funs true
    | Texp_ident (_, { txt = Lident "<>" }, _)  -> translate_eq_funs false

    | Texp_ident (_, { txt = Lident "||" }, _)  -> translate_bool_funs true
    | Texp_ident (_, { txt = Lident "&&" }, _)  -> translate_bool_funs false

    | Texp_ident (_, { txt = Lident "not" }, _) -> translate_not_fun ()

    | Texp_ident (_, { txt = Lident name }, _) -> translate_ident name expr.exp_type

    | Texp_let (flag, [bind], expr) -> translate_let flag bind expr

    | Texp_let _ -> failwith "Operator LET ... AND isn't supported" (*TODO support LET ... AND*)
    | _ -> failwith "Incorrect expression" in


  let translate_external_value_binding vb =
    if is_primary_type vb.vb_expr.exp_type
      then failwith "Primary type of external let-binding"
      else let pat  = untyper.pat untyper vb.vb_pat in
           let expr = translate_expression vb.vb_expr in
           Vb.mk pat expr in


  let mark_type_declaration td =
      match td.typ_kind with
      | Ttype_variant cds -> { td with typ_attributes = [(mknoloc "put_distrib_here", Parsetree.PStr [])] }
      | _                 -> failwith "Incrorrect type declaration" in


  let translate_structure_item stri =
    match stri.str_desc with
    | Tstr_value (rec_flag, binds) ->
      let new_binds = List.map translate_external_value_binding binds in
        Str.value rec_flag new_binds
    | Tstr_type (rec_flag, decls) ->
      let new_decls = List.map mark_type_declaration decls in
      untyper.structure_item untyper { stri with str_desc = Tstr_type (rec_flag, new_decls) }
    | _ -> failwith "Incorrect structure item" in


  let translate_structure str =
      List.map translate_structure_item str.str_items in


  translate_structure tast

(*****************************************************************************************************************************)

let add_packages ast =
  List.map (fun n -> Lident n |> mknoloc |> Opn.mk |> Str.open_) packages @ ast

(*****************************************************************************************************************************)

let beta_reductor minimal_index =

  let need_subst name arg =
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
    | _            -> failwith "Incorrect pattern in beta reduction" in

  let rec substitute expr var subst =
    match expr.pexp_desc with
    | Pexp_ident {txt = Lident name} -> if name = var then subst else expr
    | Pexp_fun (_, _, pat, body) ->
      let name = name_from_pat pat in
      if name = var then expr else substitute body var subst |> create_fun name
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
    | _ -> expr in


  let rec beta_reduction expr args =
    match expr.pexp_desc with
    | Pexp_apply (func, args') ->
      let old_args = List.map snd args' in
      let new_args = List.map (fun a -> beta_reduction a []) old_args in
      List.append new_args args |> beta_reduction func

    | Pexp_fun (_, _, pat, body) ->
      let var = match pat.ppat_desc with
                | Ppat_var v -> v.txt
                | _          -> failwith "Incorrect arg name in beta reduction" in
      begin match args with
        | arg::args' when need_subst var arg -> beta_reduction (substitute body var arg) args'
        | _                                  -> create_apply (beta_reduction body [] |> create_fun var) args
      end

    | Pexp_let (flag, vbs, expr) ->
      let new_vbs  = List.map (fun v -> { v with pvb_expr = beta_reduction v.pvb_expr [] }) vbs in
      let new_expr = beta_reduction expr args in
      Exp.let_ flag  new_vbs new_expr

    | Pexp_construct (name, Some expr) -> Some (beta_reduction expr []) |> Exp.construct name
    | Pexp_tuple args -> List.map (fun a -> beta_reduction a []) args |> Exp.tuple
    | _ -> create_apply expr args in

  let expr _ x = beta_reduction x [] in
  { Ast_mapper.default_mapper with expr }




(*****************************************************************************************************************************)

(*
let get_translator start_index need_sort_goals need_unlazy =

  (****)


  let add_rec_names l = curr_rec_names := List.append !curr_rec_names l in

  let rem_rec_names l = curr_rec_names := List.filter (fun n -> List.for_all ((<>) n) l) !curr_rec_names in

  let transl_without_rec_names f e l =
    let old_names = !curr_rec_names in
    rem_rec_names l;
    let transl_e = f e in
    curr_rec_names := old_names;
    transl_e in

  let rec expr_has_rec_vars expr = (* TODO: find in let-expressions *)
    match expr.exp_desc with
    | Texp_apply (func, args)                            -> expr_has_rec_vars func || List.exists (fun (_, Some e) -> expr_has_rec_vars e) args
    | Texp_ident (_, { txt = Longident.Lident name }, _) -> List.exists ((=) name) !curr_rec_names
    | _                                                  -> false in

  (****)



  (****)

  let rec create_fresh_argument_names_by_type (typ : Types.type_expr) need_for_res =
    match typ.desc with
    | Tarrow (_, _, right_typ, _) -> create_fresh_var_name () :: create_fresh_argument_names_by_type right_typ need_for_res
    | Tlink typ                   -> create_fresh_argument_names_by_type typ need_for_res
    | _                           -> if need_for_res then [create_fresh_var_name ()] else [] in

  (****)

  let rec path_of_longident ?(to_lower=false) = function
  | Lident s ->
      Path.Pident (Ident.create "wtf")
      (*Path.Pident (Ident.create (if to_lower
        then String.mapi (fun n -> if n=0 then Char.lowercase else fun c -> c) s
        else s^"1"))*)
  | Lapply (l,r) -> Path.Papply (path_of_longident ~to_lower l, path_of_longident ~to_lower r)
  | Ldot (t, s)  -> Path.Pdot (path_of_longident ~to_lower t, s, 0)
  in
  let rec lowercase_lident ?(to_lower=false) = function
  | Lident s -> Lident (Util.mangle_construct_name s)
  | Lapply (l, r) -> Lapply (lowercase_lident l, lowercase_lident ~to_lower r)
  | Ldot (t, s)  -> Ldot (lowercase_lident ~to_lower t, s)
  in
  let texp_unit =
    let cd =
      { Types.cstr_name = "()"; cstr_res = Obj.magic (); cstr_existentials = []
      ; cstr_args = []
      ; cstr_arity = 0
      ; cstr_tag = Cstr_constant 0
      ; cstr_consts = 1
      ; cstr_nonconsts = 0
      ; cstr_normal = 0
      ; cstr_generalized = false
      ; cstr_private = Public
      ; cstr_loc = Location.none
      ; cstr_attributes = []
#if OCAML_VERSION > (4, 02, 2)
      ; cstr_inlined = None
#endif
      } in
    Texp_construct (mknoloc @@ Lident "()", cd, []) |> expr_desc_to_expr
  in

  let rec unnest_constuct expr =
    match expr.exp_desc with
    | Texp_constant _  -> unnest_expr expr, []
    | Texp_construct ({txt=Lident s}, _, []) when s = "true" || s = "false" -> create_inj expr, []
    | Texp_construct (name, _, args) ->
      let new_args, fv = List.map unnest_constuct args |> List.split in
      let fv           = List.concat fv in
      let new_args     =
        match new_args with
        | [] -> [texp_unit]
        | l  -> l in
      let new_name     =
        match name.txt with
        | Lident "[]" -> Lident "nil"
        | Lident "::" -> Lident "%"
        | txt         -> lowercase_lident ~to_lower:true txt in
      let inj_constr   = expr_desc_to_expr @@ Texp_ident (path_of_longident name.txt, {name with txt = new_name}, dummy_val_desc) in
      let new_expr     = create_apply inj_constr new_args in
      new_expr, fv
    | _ ->
      let fr_var = create_fresh_var_name () in
      create_ident fr_var, [(fr_var, expr)] in


  let translate_construct (sub : Tast_mapper.mapper) expr =
    let output_var_name   = create_fresh_var_name () in
    let output_var        = create_ident output_var_name in

    let new_constr, fv    = unnest_constuct expr in
    let var_names         = List.map fst fv in

    let unify_cnstr       = create_unify output_var new_constr in

    let unifies_list      = List.map (fun (v, e) -> create_apply (sub.expr sub e) [create_ident v]) fv in
    let cnstr_and_unifies = List.fold_left create_and unify_cnstr unifies_list in
    let translated_cnstr  = create_fresh var_names cnstr_and_unifies in
    create_fun output_var_name translated_cnstr in

  (****)

  let translate_match (sub : Tast_mapper.mapper) expr cases (typ : Types.type_expr) =

    let argument_names = create_fresh_argument_names_by_type typ true in
    let arguments      = List.map create_ident argument_names in

    let translated_expr = sub.expr sub expr in
    let unify_var_name  = create_fresh_var_name() in
    let unify_var       = create_ident unify_var_name in
    let unify_expr      = create_apply translated_expr [unify_var] in

    let translate_case case =
      let pattern = case.c_lhs in
      let body    = case.c_rhs in

      let real_arg_pats =
        match pattern.pat_desc with
        | Tpat_constant _             -> []
        | Tpat_construct (_, _, pats) -> pats
        | _ -> raise (Error (NotYetSupported "anything other constants and constructores in the matching patterns"))
      in

      let get_var_name var =
        let Tpat_var (ident, _) = var.pat_desc in
        ident.name
      in

      let real_arg_names    = List.map get_var_name real_arg_pats in
      let real_args         = List.map create_ident real_arg_names in

      let fresh_arg_names   = List.map (fun _ -> create_fresh_var_name ()) real_args in
      let fresh_args        = List.map create_ident fresh_arg_names in

      let translated_body   = transl_without_rec_names (sub.expr sub) body real_arg_names in
      let body_with_args    = create_apply translated_body arguments in
      let body_with_lambda  = List.fold_right create_fun real_arg_names body_with_args in

      let lambda_arg_names  = List.map (fun _ -> create_fresh_var_name ()) real_args in
      let lambda_args       = List.map create_ident lambda_arg_names in

      let unifies_for_subst = List.map2 create_unify lambda_args fresh_args in
      let funs_for_subst    = List.map2 create_fun lambda_arg_names unifies_for_subst |> List.map (fun x -> [x]) in
      let body_with_subst   = List.fold_left create_apply body_with_lambda funs_for_subst in

      let cnstr             =
        match pattern.pat_desc with
        | Tpat_constant const          -> (Texp_constant const) |> expr_desc_to_expr |> create_inj
        | Tpat_construct ({txt = Lident s}, _, _) when (s = "true" || s = "false") ->
            let flid = Location.mknoloc (Lident s) in
            Texp_ident (path_of_longident (Lident s), flid, dummy_val_desc) |> expr_desc_to_expr |> create_inj
        | Tpat_construct ({txt = Lident "[]"}, _, _) -> create_apply (create_ident "nil") [texp_unit]
        | Tpat_construct ({txt = Lident "::"}, _, _) -> create_apply (create_ident "%") fresh_args
        | Tpat_construct (name, cd, [])  ->
            let flid =
              let str = PutDistrib.lower_lid {name with txt = Longident.last name.txt } in
              (* TODO: We lose module path here *)
              Location.mkloc (Lident str.txt) name.loc
            in
            create_apply
                (Texp_ident (path_of_longident flid.txt, flid, dummy_val_desc) |> expr_desc_to_expr)
                [texp_unit]
        | Tpat_construct (name, cd, _)  ->
          let flid =
            let str = PutDistrib.lower_lid {name with txt = Longident.last name.txt } in
            (* TODO: We lose module path here *)
            Location.mkloc (Lident str.txt) name.loc
          in
          create_apply
              (Texp_ident (path_of_longident flid.txt, flid, dummy_val_desc) |> expr_desc_to_expr)
              fresh_args
          in

      let cnstr_unify       = create_unify unify_var cnstr in

      let cnstr_and_body    = create_and cnstr_unify body_with_subst in

      create_fresh fresh_arg_names cnstr_and_body
    in

    let translated_cases     = cases |> List.map translate_case |> create_conde in

    let upper_exp_with_cases = if need_sort_goals && expr_has_rec_vars expr then create_and translated_cases unify_expr
                                                                            else create_and unify_expr translated_cases in

    let all_without_lambda   = create_fresh [unify_var_name] upper_exp_with_cases in

    List.fold_right create_fun argument_names all_without_lambda
  in

  (****)

  let rec is_primary_type (t : Types.type_expr) =
    match t.desc with
    | Tarrow _ -> false
    | Tlink t' -> is_primary_type t'
    | _        -> true in

  let create_unify_with_one_argument a =
    let arg = create_fresh_var_name () in
    let uni = create_unify (create_ident arg) a in
    create_fun arg uni in

  let get_pattern_names bindings =
    List.map (fun v -> v.vb_pat.pat_desc) bindings |> List.map (fun (Tpat_var (n, _)) -> n.name) in

  (****)

  let normalize_abstraction expr =
    let rec normalize_abstraction expr acc =
      match expr.exp_desc with
      | Texp_function {arg_label; param; cases=[case]; _} ->
        let Tpat_var (name, _) = case.c_lhs.pat_desc in
        let typ = case.c_lhs.pat_type in
        normalize_abstraction case.c_rhs ((name.name, typ) :: acc)
      | _ -> expr, List.rev acc in
    normalize_abstraction expr [] in


  let eta_extension expr =
    let rec get_arg_types (typ : Types.type_expr) =
      match typ.desc with
      | Tarrow (_, l, r, _) -> l :: get_arg_types r
      | Tlink typ           -> get_arg_types typ
      | _                   -> [] in

    let arg_types = get_arg_types expr.exp_type in
    List.map (fun t -> create_fresh_var_name (), t) arg_types in


  let get_pat_name p =
    match p.pat_desc with
    | Tpat_var (name, _) -> name.name in


  let two_or_more_mentions var_name expr =
    let rec two_or_more_mentions expr count =
      let eval_if_need c e = if c <= 1 then two_or_more_mentions e c else c in
      let get_pat_args p = match p.pat_desc with | Tpat_construct (_, _, pats) -> pats in

      match expr.exp_desc with
      | Texp_constant _ -> count
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
        let cases = List.filter (fun c -> List.for_all (fun p -> var_name <> get_pat_name p) @@ get_pat_args c.c_lhs) cs in
        let exprs = e :: List.map (fun c -> c.c_rhs) cases in
        List.fold_left eval_if_need count exprs in

    two_or_more_mentions expr 0 >= 2 in


  let translate_function_body (sub : Tast_mapper.mapper) body real_vars =
    let eta_vars                = eta_extension body in
    let translated_body         = sub.expr sub body in

    let result_var              = create_fresh_var_name () in
    let body_with_eta_args      = create_apply translated_body @@ List.map (fun (n, _) -> create_ident n) eta_vars @ [create_ident result_var] in

    let primary_vars_with_types = List.filter (fun (_, t) -> is_primary_type t) @@ real_vars @ eta_vars in
    let primary_vars            = List.map fst primary_vars_with_types in
    let bad_vars                = List.filter (fun v -> two_or_more_mentions v body) primary_vars in

    let fresh_vars              = List.map (fun _ -> create_fresh_var_name ()) bad_vars in
    let absr_body               = List.fold_right create_fun bad_vars body_with_eta_args in
    let body_with_ags           = create_apply absr_body @@ List.map (fun x -> create_unify_with_one_argument @@ create_ident x) fresh_vars in

    let conjs                   = List.map2 (fun a b -> create_apply (create_ident a) @@ [create_ident b]) bad_vars fresh_vars in
    let full_conj               = List.fold_right create_and conjs body_with_ags in
    let with_fresh              = create_fresh fresh_vars full_conj in
    let first_fun               = create_fun result_var with_fresh in

    List.fold_right create_fun (List.map fst eta_vars) first_fun in


  let translate_abstraction (sub : Tast_mapper.mapper) expression =
    let body, real_vars = normalize_abstraction expression in
    let new_body        = translate_function_body sub body real_vars in
    List.fold_right create_fun (List.map fst real_vars) new_body in

  (****)

  let translate_nonrec_let (sub : Tast_mapper.mapper) bindings expr typ =
    let real_vars = List.map (fun b -> get_pat_name b.vb_pat, b.vb_pat.pat_type) bindings in
    let new_expr  = translate_function_body sub expr real_vars in
    let new_binds = List.map (fun b -> { b with vb_expr = sub.expr sub b.vb_expr }) bindings in
    expr_desc_to_expr (Texp_let (Nonrecursive, new_binds, new_expr)) in

  (****)

  let translate_if (sub : Tast_mapper.mapper) cond th el typ =
    let argument_names = create_fresh_argument_names_by_type typ true in
    let arguments      = List.map create_ident argument_names in

    let cond_var_name = create_fresh_var_name () in
    let cond_var      = create_ident cond_var_name in

    let create_branch case branch =
      let tanslated_branch = sub.expr sub branch in
      let branch_with_args = create_apply tanslated_branch arguments in
      let case_checker     = create_unify cond_var case in
      create_and case_checker branch_with_args in


   let const_true  = create_constructor true_name [] |> create_inj in
   let const_false = create_constructor false_name [] |> create_inj in
   let both_cases = create_conde [create_branch const_true th; create_branch const_false el] in

   let translated_cond = create_apply (sub.expr sub cond) [cond_var] in
   let cond_with_cases = if need_sort_goals && expr_has_rec_vars cond
                         then create_and both_cases translated_cond
                         else create_and translated_cond both_cases in
   let body_with_fresh = create_fresh [cond_var_name] cond_with_cases in
   List.fold_right create_fun argument_names body_with_fresh in

  (****)

  let translate_eq is_equality =

    let name_arg_l  = create_fresh_var_name () in
    let name_arg_r  = create_fresh_var_name () in
    let name_out    = create_fresh_var_name () in
    let name_l      = create_fresh_var_name () in
    let name_r      = create_fresh_var_name () in

    let ident_arg_l = create_ident name_arg_l in
    let ident_arg_r = create_ident name_arg_r in
    let ident_out   = create_ident name_out in
    let ident_l     = create_ident name_l in
    let ident_r     = create_ident name_r in

    let const_true  = create_constructor true_name [] |> create_inj in
    let const_false = create_constructor false_name [] |> create_inj in

    let out_unify_eq    = create_unify ident_out (if is_equality then const_true else const_false) in
    let out_unify_neq   = create_unify ident_out (if is_equality then const_false else const_true) in
    let l_unify_r       = create_unify ident_l ident_r in
    let l_des_constr_r  = create_des_constr ident_l ident_r in
    let first_and       = create_and l_unify_r out_unify_eq in
    let second_and      = create_and l_des_constr_r out_unify_neq in
    let full_or         = create_or first_and second_and in

    let apply_l = create_apply ident_arg_l [ident_l] in
    let apply_r = create_apply ident_arg_r [ident_r] in

    let party_and = create_and apply_r full_or in
    let full_and  = create_and apply_l party_and in

    let fresh  = create_fresh [name_l; name_r] full_and in

    let fun1 = create_fun name_out fresh in
    let fun2 = create_fun name_arg_r fun1 in

    create_fun name_arg_l fun2 in

  (****)

  let translate_bool_funs is_or =

    let name_arg_l  = create_fresh_var_name () in
    let name_arg_r  = create_fresh_var_name () in
    let name_out    = create_fresh_var_name () in
    let name_l      = create_fresh_var_name () in

    let ident_arg_l = create_ident name_arg_l in
    let ident_arg_r = create_ident name_arg_r in
    let ident_out   = create_ident name_out in
    let ident_l     = create_ident name_l in

    let const_true  = create_constructor true_name [] |> create_inj in
    let const_false = create_constructor false_name [] |> create_inj in

    let l_true    = create_unify ident_l (if is_or then const_true else const_false) in
    let l_false   = create_unify ident_l (if is_or then const_false else const_true) in
    let q_true    = create_unify ident_out (if is_or then const_true else const_false) in
    let calc_r    = create_apply ident_arg_r [ident_out] in

    let first_and       = create_and l_true q_true in
    let second_and      = create_and l_false calc_r in
    let full_or         = create_or first_and second_and in

    let apply_l = create_apply ident_arg_l [ident_l] in

    let full_and  = create_and apply_l full_or in

    let with_fresh = create_fresh [name_l] full_and in

    let fun1 = create_fun name_out with_fresh in
    let fun2 = create_fun name_arg_r fun1 in

    create_fun name_arg_l fun2 in

  let translate_not =
    let name_arg    = create_fresh_var_name () in
    let name_out    = create_fresh_var_name () in
    let fr_arg      = create_fresh_var_name () in

    let ident_arg   = create_ident name_arg in
    let ident_out   = create_ident name_out in
    let fr_ident    = create_ident fr_arg in

    let const_true  = create_constructor true_name [] |> create_inj in
    let const_false = create_constructor false_name [] |> create_inj in

    let arg_true    = create_unify fr_ident const_true in
    let arg_false   = create_unify fr_ident const_false in
    let out_true    = create_unify ident_out const_true in
    let out_false   = create_unify ident_out const_false in

    let fst_and     = create_and arg_true out_false in
    let snd_and     = create_and arg_false out_true in
    let full_or     = create_or fst_and snd_and in

    let apply       = create_apply ident_arg [fr_ident] in
    let full_and    = create_and apply full_or in

    let with_fresh  = create_fresh [fr_arg] full_and in
    let fun1        = create_fun name_out with_fresh in
    create_fun name_arg fun1 in

  (****)

  let translate_letrec_bindings_with_tibling sub binds =
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

    add_rec_names (get_pattern_names binds);

    let translate_binding bind =
      let body            = bind.vb_expr in
      let transtated_body = sub.expr sub body in
      let typ             = body.exp_type in

      let has_tabled_attr = List.exists (fun a -> (fst a).txt = tabling_attr_name) bind.vb_attributes in


      if not has_tabled_attr || has_func_arg typ then let vb_expr = transtated_body in { bind with vb_expr } else
        let Tpat_var (name, _)    = bind.vb_pat.pat_desc in
        let unrec_body            = create_fun name.name transtated_body in

        let recfunc_argument_name = create_fresh_var_name () in
        let recfunc_argument      = create_ident recfunc_argument_name in

        let argument_names1       = create_fresh_argument_names_by_type typ false in
        let arguments1            = List.map create_ident argument_names1 in
        let res_arg_name_1        = create_fresh_var_name () in
        let rec_arg_1             = create_ident res_arg_name_1 in

        let argument_names2       = create_fresh_argument_names_by_type typ false in
        let arguments2            = List.map create_ident argument_names2 in

        let recfunc_with_args     = create_apply recfunc_argument (List.append arguments2 [rec_arg_1]) in
        let conjuncts1            = List.map2 (fun q1 q2 -> create_apply q1 [q2]) arguments1 arguments2 in
        let conjs_and_recfunc     = List.fold_right create_and conjuncts1 recfunc_with_args in
        let freshing_and_recfunc  = create_fresh argument_names2 conjs_and_recfunc in
        let lambdas_and_recfunc   = List.fold_right create_fun (List.append argument_names1 [res_arg_name_1]) freshing_and_recfunc in

        let argument_names3       = create_fresh_argument_names_by_type typ false in
        let arguments3            = List.map create_ident argument_names3 in

        let argument_names4       = create_fresh_argument_names_by_type typ false in
        let arguments4            = List.map create_ident argument_names4 in
        let unified_vars1         = List.map2 create_unify arguments3 arguments4 in
        let lambda_vars1          = List.map2 create_fun argument_names3 unified_vars1 in

        let new_nody              = create_apply unrec_body (lambdas_and_recfunc :: lambda_vars1) in
        let lambda_new_body       = List.fold_right create_fun (recfunc_argument_name :: argument_names4) new_nody in

        let succ                  = create_ident_with_dot tabling_module_name tabling_succ_name in
        let one                   = create_ident_with_dot tabling_module_name tabling_one_name in
        let tabledrec             = create_ident_with_dot tabling_module_name tabling_rec_name in

        let rec get_tabling_rank (typ : Types.type_expr) =
          match typ.desc with
          | Tarrow (_, _, right_typ, _) -> create_apply succ [get_tabling_rank right_typ]
          | Tlink typ                   -> get_tabling_rank typ
          | _                           -> one in

        let tabling_rank          = get_tabling_rank typ in
        let tabled_body           = create_apply tabledrec [tabling_rank; lambda_new_body] in

        let argument_names5       = create_fresh_argument_names_by_type typ false in
        let arguments5            = List.map create_ident argument_names5 in
        let res_arg_name_5        = create_fresh_var_name () in
        let rec_arg_5             = create_ident res_arg_name_5 in

        let argument_names6       = create_fresh_argument_names_by_type typ false in
        let arguments6            = List.map create_ident argument_names6 in

        let tabled_body_with_args = create_apply tabled_body (List.append arguments6 [rec_arg_5]) in

        let conjuncts2            = List.map2 (fun q1 q2 -> create_apply q1 [q2]) arguments5 arguments6 in
        let conjs_and_tabled      = List.fold_right create_and conjuncts2 tabled_body_with_args in
        let freshing_and_tabled   = create_fresh argument_names6 conjs_and_tabled in
        let lambdas_and_tabled    = List.fold_right create_fun (List.append argument_names5 [res_arg_name_5]) freshing_and_tabled in

        let vb_expr = lambdas_and_tabled in
        { bind with vb_expr } in

    List.map translate_binding binds in

  (****)

  let structure_item sub x =
    match x.str_desc with
    | Tstr_value (Recursive, bingings) ->
      let new_bindings = translate_letrec_bindings_with_tibling sub bingings in
      let str_desc = Tstr_value (Recursive, new_bindings) in
      { x with str_desc }
    | _ -> Tast_mapper.default.structure_item sub x in

  let expr sub x = (*TODO: Update rec names for abstraction*)
    match x.exp_desc with
    | Texp_constant _           -> translate_construct sub x
    | Texp_construct (_, _, _)  -> translate_construct sub x
#if OCAML_VERSION > (4, 02, 2)
    | Texp_match (e, cs, _, _)  -> translate_match sub e cs x.exp_type
#else
    | Texp_match (e, cs, _, _, _)  -> translate_match sub e cs x.exp_type
#endif
    | Texp_ifthenelse (cond, th, Some el) -> translate_if sub cond th el x.exp_type

    | Texp_function _ when need_unlazy -> translate_abstraction sub x

    | Texp_ident (_, { txt = Longident.Lident name }, _) when name = eq_name -> translate_eq true

    | Texp_ident (_, { txt = Longident.Lident name }, _) when name = neq_name -> translate_eq false

    | Texp_ident (_, { txt = Longident.Lident name }, _) when name = bool_or_name  -> translate_bool_funs true

    | Texp_ident (_, { txt = Longident.Lident name }, _) when name = bool_and_name -> translate_bool_funs false

    | Texp_ident (_, { txt = Longident.Lident name }, _) when name = bool_not_name -> translate_not

    | Texp_let (Recursive, bindings, expr) ->
      let new_bindings = translate_letrec_bindings_with_tibling sub bindings in
      let transl_expr  = sub.expr sub expr in
      rem_rec_names (get_pattern_names bindings);
      expr_desc_to_expr (Texp_let (Recursive, new_bindings, transl_expr))

    | Texp_let (Nonrecursive, bindings, expr) -> translate_nonrec_let sub bindings expr x.exp_type

    | _ -> Tast_mapper.default.expr sub x in

  let check_cd_is_ok (_cd: constructor_declaration list) = true in

  { Tast_mapper.default with expr; structure_item
  (* ; type_kind = fun _ ->  *)
  ; type_declaration = fun _ decl ->
      match decl.typ_kind with
      | Ttype_variant cds when check_cd_is_ok cds ->
          { decl with typ_attributes = [(Location.mknoloc "put_distrib_here", Parsetree.PStr [])] }
      | Ttype_record _  -> raise (Error (NotYetSupported "record types"))
      | Ttype_abstract  -> raise (Error (NotYetSupported "abstract types"))
      | Ttype_open      -> raise (Error (NotYetSupported "open types"))
}

*)
end

let print_if ppf flag printer arg =
  if !flag then Format.fprintf ppf "%a@." printer arg;
  arg

let eval_if_need flag f =
  if flag then f else fun x -> x

let only_generate ~oldstyle hook_info tast =
  if oldstyle
  then try
    let open Lozov  in
    let need_reduce = false in
    let start_index = get_max_index tast in
    let reductor    = beta_reductor start_index in
    translate tast start_index |>
    add_packages |>
    eval_if_need need_reduce (reductor.structure reductor) |>
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
