open Asttypes
open Longident
open Typedtree
open Ast_helper
open Ident
open Parsetree
open Util

(**************************************************************************)

let translate tast start_index params =
  let lowercase_lident x = if params.leave_constuctors then x else lowercase_lident x in
  let curr_index = ref start_index in
  let create_fresh_var_name () =
    let name = Printf.sprintf "%s%d" fresh_var_prefix !curr_index in
    incr curr_index;
    name
  in
  let rec create_fresh_argument_names_by_type (typ : Types.type_expr) =
    match Types.get_desc typ with
    | Tarrow (_, _, right_typ, _) ->
      create_fresh_var_name () :: create_fresh_argument_names_by_type right_typ
    | Tlink typ -> create_fresh_argument_names_by_type typ
    | _ -> [ create_fresh_var_name () ]
  in
  (*************************************************)
  let rec unnest_expr let_vars expr =
    let loc = expr.exp_loc in
    match expr.exp_desc with
    | Texp_ident (_, { txt = Longident.Lident name }, _)
      when List.for_all (( <> ) name) let_vars -> untyper.expr untyper expr, []
    | Texp_constant c -> create_inj (Exp.constant (Untypeast.constant c)), []
    | Texp_construct ({ txt = Lident s }, _, []) when s = "true" || s = "false" ->
      create_inj (untyper.expr untyper expr), []
    | Texp_tuple [ a; b ] ->
      let new_args, fv = List.map (unnest_expr let_vars) [ a; b ] |> List.split in
      let fv = List.concat fv in
      create_apply (mark_constr [%expr pair]) new_args, fv
    | Texp_construct (name, desc, args) ->
      let args =
        match get_constr_args loc desc args with
        | `Constr_args args -> args
        | _ -> failwith "Inlined records unsupported"
      in
      let new_args, fv = List.map (unnest_expr let_vars) args |> List.split in
      let fv = List.concat fv in
      let new_args =
        match new_args with
        | [] -> [ [%expr ()] ]
        | l -> l
      in
      let new_name =
        match name.txt with
        | Lident "[]" -> Lident "nil"
        | Lident "::" -> Lident "%"
        | txt -> lowercase_lident txt
      in
      create_apply (mknoloc new_name |> Exp.ident |> mark_constr) new_args, fv
    | _ when is_primary_type expr.exp_type ->
      let fr_var = create_fresh_var_name () in
      create_id fr_var, [ fr_var, expr ]
    | _ -> translate_expression let_vars expr, []
  and translate_construct let_vars expr =
    let constr, binds = unnest_expr let_vars expr in
    let out_var_name = create_fresh_var_name () in
    let unify_constr =
      let loc = Ppxlib.Location.none in
      [%expr [%e create_id out_var_name] === [%e constr]]
    in
    let conjs =
      unify_constr
      :: List.map
           (fun (v, e) -> create_apply (translate_expression let_vars e) [ create_id v ])
           binds
    in
    let conj = create_conj conjs in
    let with_fresh = List.fold_right create_fresh (List.map fst binds) conj in
    create_fun out_var_name with_fresh
  and translate_ident let_vars name typ =
    if is_primary_type typ && List.for_all (( <> ) name) let_vars
    then (
      let var = create_fresh_var_name () in
      let loc = Ppxlib.Location.none in
      [%expr fun [%p create_pat var] -> [%e create_id name] === [%e create_id var]])
    else create_id name
  and translate_abstraciton let_vars case =
    let let_vars = filter_vars let_vars [ get_pat_name case.c_lhs ] in
    Exp.fun_
      Nolabel
      None
      (untyper.pat untyper case.c_lhs)
      (translate_expression let_vars case.c_rhs)
  and normalize_apply expr =
    match expr.exp_desc with
    | Texp_apply (f, args_r) ->
      let expr', args_l = normalize_apply f in
      ( expr'
      , args_l
        @ List.map
            (function
             | _, Some x -> x
             | _ -> fail_loc expr.exp_loc "Incorrect argument")
            args_r )
    | _ -> expr, []
  and translate_apply let_vars expr =
    let f, args = normalize_apply expr in
    let new_args, binds = List.map (unnest_expr let_vars) args |> List.split in
    let binds = List.concat binds in
    if List.length binds = 0
    then create_apply (translate_expression let_vars f) new_args
    else (
      let eta_vars = create_fresh_argument_names_by_type expr.exp_type in
      let eta_call =
        create_apply
          (translate_expression let_vars f)
          (new_args @ List.map create_id eta_vars)
      in
      let conjs =
        List.map
          (fun (v, e) -> create_apply (translate_expression let_vars e) [ create_id v ])
          binds
        @ [ eta_call ]
      in
      let full_conj = create_conj conjs in
      let with_fresh = List.fold_right create_fresh (List.map fst binds) full_conj in
      List.fold_right create_fun eta_vars with_fresh)
  and eta_form_for_let let_part let_vars expr =
    let transl_expr = translate_expression let_vars expr in
    if is_primary_type expr.exp_type
    then (
      let new_var = create_fresh_var_name () in
      let loc = Ppxlib.Location.none in
      [%expr
        fun [%p create_pat new_var] ->
          [%e create_apply transl_expr [ create_id new_var ] |> let_part]])
    else let_part transl_expr
  and translate_nonrec_let let_vars bind expr =
    if (not params.unnesting_params.polymorphism_supported)
       && is_primary_type bind.vb_expr.exp_type
    then (
      let name = get_pat_name bind.vb_pat in
      let conj1 =
        create_apply (translate_expression let_vars bind.vb_expr) [ create_id name ]
      in
      let args = create_fresh_argument_names_by_type expr.exp_type in
      let conj2 =
        create_apply (translate_expression let_vars expr) (List.map create_id args)
      in
      let both = create_conj [ conj1; conj2 ] in
      let with_fresh = create_fresh name both in
      List.fold_right create_fun args with_fresh)
    else (
      let new_let_vars =
        if is_primary_type bind.vb_expr.exp_type
        then get_pat_name bind.vb_pat :: let_vars
        else let_vars
      in
      let let_part =
        Exp.let_
          Nonrecursive
          [ Vb.mk
              (untyper.pat untyper bind.vb_pat)
              (translate_expression let_vars bind.vb_expr)
          ]
      in
      eta_form_for_let let_part new_let_vars expr)
  and translate_rec_let let_vars bind expr =
    let rec get_tabling_rank (typ : Types.type_expr) =
      let loc = Ppxlib.Location.none in
      match Types.get_desc typ with
      | Tarrow (_, _, right_typ, _) ->
        create_apply [%expr Tabling.succ] [ get_tabling_rank right_typ ]
      | Tlink typ -> get_tabling_rank typ
      | _ -> [%expr Tabling.one]
    in
    let body = translate_expression let_vars bind.vb_expr in
    let typ = bind.vb_expr.exp_type in
    let has_tabled_attr =
      List.exists (fun a -> a.attr_name.txt = tabling_attr_name) bind.vb_attributes
    in
    if not has_tabled_attr
    then (
      let let_part =
        Exp.let_ Recursive [ Vb.mk (untyper.pat untyper bind.vb_pat) body ]
      in
      eta_form_for_let let_part let_vars expr)
    else if has_func_arg typ
    then fail_loc bind.vb_loc "Tabled function has functional argument"
    else (
      let name = get_pat_name bind.vb_pat in
      let abst = create_fun name body in
      let rank = get_tabling_rank typ in
      let loc = Ppxlib.Location.none in
      let appl = create_apply [%expr Tabling.tabledrec] [ rank; abst ] in
      let let_part =
        Exp.let_ Nonrecursive [ Vb.mk (untyper.pat untyper bind.vb_pat) appl ]
      in
      eta_form_for_let let_part let_vars expr)
  and translate_let let_vars flag bind expr =
    let bind = { bind with vb_pat = normalize_let_name bind.vb_pat } in
    match flag with
    | Recursive -> translate_rec_let let_vars bind expr
    | Nonrecursive -> translate_nonrec_let let_vars bind expr
  and translate_match (type a) let_vars loc expr (cases : a Typedtree.case list) typ =
    let args = create_fresh_argument_names_by_type typ in
    let scrutinee_is_var =
      match expr.exp_desc with
      | Texp_ident (_, { txt = Longident.Lident name }, _) ->
        not @@ List.mem name let_vars
      | _ -> false
    in
    let scrutinee_var =
      match expr.exp_desc with
      | Texp_ident (_, { txt = Longident.Lident name }, _) ->
        if scrutinee_is_var then name else create_fresh_var_name ()
      | Texp_ident _ -> fail_loc expr.exp_loc "Incorrect variable"
      | _ -> create_fresh_var_name ()
    in
    let rec rename var1 var2 pat =
      match pat.pexp_desc with
      | Pexp_ident { txt = Lident name } -> if name = var1 then create_id var2 else pat
      | Pexp_apply (f, args) ->
        List.map snd args |> List.map (rename var1 var2) |> create_apply f
      | _ -> pat
    in
    let translate_case (type a) (case : a Typedtree.case) : _ =
      let pat, als, vars = translate_pat case.c_lhs create_fresh_var_name in
      let is_overlap = List.mem scrutinee_var vars in
      let new_var = if is_overlap then create_fresh_var_name () else "" in
      let pat = if is_overlap then rename scrutinee_var new_var pat else pat in
      let vars =
        if is_overlap
        then List.map (fun n -> if n = scrutinee_var then new_var else n) vars
        else vars
      in
      let unify = [%expr [%e create_id scrutinee_var] === [%e pat]] in
      let unifies = List.map alias2unify als in
      let body =
        create_apply
          (translate_expression (filter_vars let_vars vars) case.c_rhs)
          (List.map create_id args)
      in
      let body =
        if is_overlap
        then create_apply (create_fun scrutinee_var body) [ create_id new_var ]
        else body
      in
      let conj = create_conj ((unify :: unifies) @ [ body ]) in
      List.fold_right create_fresh vars conj
    in
    if cases |> List.map (fun c -> c.c_lhs) |> is_disj_pats
    then (
      let new_cases = List.map translate_case cases in
      let disj = create_disj new_cases in
      let with_fresh =
        if scrutinee_is_var
        then disj
        else
          create_conj
            [ create_apply
                (translate_expression let_vars expr)
                [ create_id scrutinee_var ]
            ; disj
            ]
          |> create_fresh scrutinee_var
      in
      List.fold_right create_fun args with_fresh)
    else fail_loc loc "Pattern matching contains unified patterns"
  and translate_bool_funs is_or =
    let loc = Ppxlib.Location.none in
    if params.unnesting_params.use_standart_bool_relations
    then if is_or then [%expr Bool.oro] else [%expr Bool.ando]
    else (
      let a1 = create_fresh_var_name () in
      let a2 = create_fresh_var_name () in
      let q = create_fresh_var_name () in
      let fst = if is_or then [%expr !!true] else [%expr !!false] in
      let snd = if is_or then [%expr !!false] else [%expr !!true] in
      if params.unnesting_params.remove_false
      then
        if is_or
        then
          [%expr
            fun [%p create_pat a1] [%p create_pat a2] [%p create_pat q] ->
              [%e create_id q]
              === !!true
              &&& ([%e create_id a1] === !!true ||| ([%e create_id a2] === !!true))]
        else
          [%expr
            fun [%p create_pat a1] [%p create_pat a2] [%p create_pat q] ->
              [%e create_id q]
              === !!true
              &&& ([%e create_id a1] === !!true &&& ([%e create_id a2] === !!true))]
      else
        [%expr
          fun [%p create_pat a1] [%p create_pat a2] [%p create_pat q] ->
            conde
              [ [%e create_id a1] === [%e fst] &&& ([%e create_id q] === [%e fst])
              ; [%e create_id a1] === [%e snd] &&& ([%e create_id q] === [%e create_id a2])
              ]])
  and translate_eq_funs is_eq =
    let a1 = create_fresh_var_name () in
    let a2 = create_fresh_var_name () in
    let q = create_fresh_var_name () in
    let loc = Ppxlib.Location.none in
    let fst = if is_eq then [%expr !!true] else [%expr !!false] in
    let snd = if is_eq then [%expr !!false] else [%expr !!true] in
    if params.unnesting_params.remove_false
    then
      [%expr
        fun [%p create_pat a1] [%p create_pat a2] [%p create_pat q] ->
          [%e create_id a1] === [%e create_id a2] &&& ([%e create_id q] === [%e fst])]
    else
      [%expr
        fun [%p create_pat a1] [%p create_pat a2] [%p create_pat q] ->
          conde
            [ [%e create_id a1] === [%e create_id a2] &&& ([%e create_id q] === [%e fst])
            ; [%e create_id a1] =/= [%e create_id a2] &&& ([%e create_id q] === [%e snd])
            ]]
  and translate_not_fun () =
    let loc = Ppxlib.Location.none in
    if params.unnesting_params.use_standart_bool_relations
    then [%expr Bool.noto]
    else (
      let a = create_fresh_var_name () in
      let q = create_fresh_var_name () in
      [%expr
        fun [%p create_pat a] [%p create_pat q] ->
          conde
            [ [%e create_id a] === !!true &&& ([%e create_id q] === !!false)
            ; [%e create_id a] === !!false &&& ([%e create_id q] === !!true)
            ]])
  and translate_if let_vars cond th el typ =
    let args = create_fresh_argument_names_by_type typ in
    let cond_is_var =
      match cond.exp_desc with
      | Texp_ident _ -> true
      | _ -> false
    in
    let cond_var =
      match cond.exp_desc with
      | Texp_ident (_, { txt = Longident.Lident name }, _) -> name
      | Texp_ident _ -> fail_loc cond.exp_loc "Incorrect variable"
      | _ -> create_fresh_var_name ()
    in
    let th = create_apply (translate_expression let_vars th) (List.map create_id args) in
    let el = create_apply (translate_expression let_vars el) (List.map create_id args) in
    let loc = Ppxlib.Location.none in
    let body =
      [%expr
        conde
          [ [%e create_id cond_var] === !!true &&& [%e th]
          ; [%e create_id cond_var] === !!false &&& [%e el]
          ]]
    in
    let with_fresh =
      if cond_is_var
      then body
      else
        [%expr
          call_fresh (fun [%p create_pat cond_var] ->
            [%e translate_expression let_vars cond] [%e create_id cond_var] &&& [%e body])]
    in
    List.fold_right create_fun args with_fresh
  and translate_let_star let_vars loc t let_ body =
    if let_.bop_op_name.txt = source_bind_name
    then (
      let var = get_pat_name @@ body.c_lhs in
      let exp_in = translate_expression let_vars body.c_rhs in
      let body, binds = unnest_expr let_vars let_.bop_exp in
      let new_args = [ body; create_fun var exp_in ] in
      if List.length binds = 0
      then create_apply (create_id bind_name) new_args
      else (
        let eta_vars = create_fresh_argument_names_by_type t in
        let eta_call =
          create_apply (create_id bind_name) (new_args @ List.map create_id eta_vars)
        in
        let conjs =
          List.map
            (fun (v, e) -> create_apply (translate_expression let_vars e) [ create_id v ])
            binds
          @ [ eta_call ]
        in
        let full_conj = create_conj conjs in
        let with_fresh = List.fold_right create_fresh (List.map fst binds) full_conj in
        List.fold_right create_fun eta_vars with_fresh))
    else fail_loc loc "Unexpected let operation (only 'let*' is supported)"
  and translate_expression let_vars expr =
    match expr.exp_desc with
    | Texp_constant _ -> translate_construct let_vars expr
    | Texp_construct _ -> translate_construct let_vars expr
    | Texp_tuple [ _; _ ] -> translate_construct let_vars expr
    | Texp_apply _ -> translate_apply let_vars expr
    | Texp_match (e, cs, _) -> translate_match let_vars expr.exp_loc e cs expr.exp_type
    | Texp_ifthenelse (cond, th, Some el) ->
      if params.unnesting_params.remove_false
      then fail_loc expr.exp_loc "if-then-else expression in not-false mode"
      else translate_if let_vars cond th el expr.exp_type
    | Texp_function { cases = [ case ] } -> translate_abstraciton let_vars case
    | Texp_ident (_, { txt = Lident "=" }, _) -> translate_eq_funs true
    | Texp_ident (_, { txt = Lident "<>" }, _) ->
      if params.unnesting_params.remove_false
      then fail_loc expr.exp_loc "Operator '<>' in not-false mode"
      else translate_eq_funs false
    | Texp_ident (_, { txt = Lident "||" }, _) -> translate_bool_funs true
    | Texp_ident (_, { txt = Lident "&&" }, _) -> translate_bool_funs false
    | Texp_ident (_, { txt = Lident "not" }, _) ->
      if params.unnesting_params.remove_false
      then fail_loc expr.exp_loc "Operator 'not' in not-false mode"
      else translate_not_fun ()
    | Texp_ident (_, { txt = Lident name }, _) ->
      translate_ident let_vars name expr.exp_type
    | Texp_ident (_, { txt }, _) -> mknoloc txt |> Exp.ident
    | Texp_let (flag, [ bind ], expr) -> translate_let let_vars flag bind expr
    | Texp_let _ ->
      fail_loc
        expr.exp_loc
        "Operator LET ... AND isn't supported" (*TODO support LET ... AND*)
    | Texp_letop { let_; body } ->
      translate_let_star let_vars expr.exp_loc expr.exp_type let_ body
    | _ -> fail_loc expr.exp_loc "Incorrect expression"
  in
  let translate_external_value_binding let_vars vb =
    let pat = untyper.pat untyper (normalize_let_name vb.vb_pat) in
    let expr = translate_expression let_vars vb.vb_expr in
    Vb.mk pat expr
  in
  let translate_structure_item let_vars stri =
    match stri.str_desc with
    | Tstr_value (rec_flag, [ bind ]) ->
      Str.value rec_flag [ translate_external_value_binding let_vars bind ]
    | Tstr_type (rec_flag, decls) ->
      let new_decls = List.map mark_type_declaration decls in
      untyper.structure_item
        untyper
        { stri with str_desc = Tstr_type (rec_flag, new_decls) }
    | Tstr_open _ -> untyper.structure_item untyper stri
    | _ -> fail_loc stri.str_loc "Incorrect structure item"
  in
  let translate_structure str =
    let rec translate_items let_vars = function
      | [] -> []
      | x :: xs ->
        let new_let_vars =
          match x.str_desc with
          | Tstr_value
              (Nonrecursive, [ { vb_expr; vb_pat = { pat_desc = Tpat_var (var, _) } } ])
            when is_primary_type vb_expr.exp_type -> name var :: let_vars
          | _ -> let_vars
        in
        translate_structure_item let_vars x :: translate_items new_let_vars xs
    in
    translate_items [] str.str_items
  in
  translate_structure tast
;;

let only_generate tast params =
  try
    let start_index = get_max_index tast in
    let reductor = Beta_reductor.beta_reductor start_index params.subst_only_util_vars in
    translate tast start_index params
    |> add_packages
    |> eval_if_need params.beta_reduction (reductor.structure reductor)
    |> eval_if_need
         params.normalization
         (let mapper = Normalizer.fresh_and_conjs_normalizer params in
          mapper.structure mapper)
    |> eval_if_need
         params.high_order_paprams.use_call_by_need
         Call_by_need.call_by_need_creator
    |> Put_distrib.process params
    |> print_if Format.std_formatter Clflags.dump_parsetree Printast.implementation
    |> print_if Format.std_formatter Clflags.dump_source Pprintast.structure
  with
  | TranslatorError e as exc ->
    report_error Format.std_formatter e;
    raise exc
;;
