open Longident
open Typedtree
open Ast_helper
open Ident
open Parsetree

open Util

(**************************************************************************)

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
          | Tpat_record (l, _)          -> List.concat @@ List.map (fun (_, _, p) -> get_pat_vars p) l
          | Tpat_alias (t, n, _)        -> name n :: get_pat_vars t in

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
        let pat, als, vars = translate_pat case.c_lhs create_fresh_var_name in
        let unify          = [%expr [%e create_id scrutinee_var] === [%e pat]] in
        let unifies        = List.map (fun (v, p) -> [%expr [%e v] === [%e p]]) als in
        let body           = create_apply (translate_expression case.c_rhs) (List.map create_id extra_args) in
        let abst_body      = List.fold_right create_fun vars body in
        let subst          = List.map create_subst vars in
        let total_body     = create_apply abst_body subst in
        let conj           = create_conj (unify :: unifies @ [total_body]) in
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
