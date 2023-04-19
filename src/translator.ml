open Longident
open Typedtree
open Ast_helper
open Ident
open Parsetree
open Util

let () = Printexc.record_backtrace true

(*****************************************************************************************************************************)

let translate_high tast start_index params =
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
    | _ -> []
  in
  let rec create_fresh_argument_names_by_args (typ : Types.type_expr) =
    match Types.get_desc typ with
    | Tarrow (_, _, right_typ, _) ->
      create_fresh_var_name () :: create_fresh_argument_names_by_args right_typ
    | Tlink typ -> create_fresh_argument_names_by_args typ
    | _ -> []
  in
  let rec unnest_constuct e =
    let loc = Ppxlib.Location.none in
    match e.exp_desc with
    | Texp_constant (Const_int i) ->
      [%expr from_int [%e Untypeast.untype_expression e]], [], []
    | Texp_constant c -> create_inj (Exp.constant (Untypeast.constant c)), [], []
    | Texp_construct ({ txt = Lident s }, _, [])
      when s = "true" || s = "false" || s = "()" ->
      create_inj (untyper.expr untyper e), [], []
    | Texp_construct ({ txt = Lident s }, _, []) when s = "Nothing" ->
      let new_var = create_fresh_var_name () in
      create_id new_var, [ FailureExpr ], [ new_var ]
    | Texp_construct ({ txt = Lident s }, _, [ a ]) when s = "Just" -> unnest_constuct a
    | Texp_tuple l ->
      let new_args, als, vars = List.map unnest_constuct l |> split3 in
      ( fold_right1
          (fun e1 e2 -> create_apply (mark_constr [%expr Std.pair]) [ e1; e2 ])
          new_args
      , List.concat als
      , List.concat vars )
    | Texp_construct (name, desc, args) ->
      let name = add_translated_module_name_in_ident name in
      (match get_constr_args loc desc args with
       | `Constr_args args ->
         let new_args, als, vars = List.map unnest_constuct args |> split3 in
         create_constr name new_args, List.concat als, List.concat vars
       | `Inlined_record_fields fields ->
         let new_args, als, vars =
           List.map (fun (_, expr) -> unnest_constuct expr) fields |> split3
         in
         let bindings = List.map2 (fun (l, _) e -> l, e) fields new_args in
         create_constr name [ create_record bindings ], List.concat als, List.concat vars)
    | _ ->
      let fr_var = create_fresh_var_name () in
      create_id fr_var, [ Call (create_id fr_var, translate_expression e) ], [ fr_var ]
  and translate_construct expr =
    let loc = Ppxlib.Location.none in
    let constr, binds, vars = unnest_constuct expr in
    let out_var_name = create_fresh_var_name () in
    let unify_constr = [%expr [%e create_id out_var_name] === [%e constr]] in
    let conjs = unify_constr :: List.map alias2unify binds in
    let conj = create_conj conjs in
    let with_fresh = List.fold_right create_fresh vars conj in
    [%expr fun [%p create_logic_var out_var_name] -> [%e with_fresh]]
  and translate_bool_funs_without_false is_or =
    let a1 = create_fresh_var_name () in
    let a2 = create_fresh_var_name () in
    let q = create_fresh_var_name () in
    let loc = Ppxlib.Location.none in
    let op = if is_or then [%expr ( ||| )] else [%expr ( &&& )] in
    [%expr
      fun [%p create_pat a1] [%p create_pat a2] [%p create_logic_var q] ->
        [%e create_id q]
        === !!true
        &&& [%e op] ([%e create_id a1] !!true) ([%e create_id a2] !!true)]
  and translate_bool_funs_with_false is_or =
    let a1 = create_fresh_var_name () in
    let a2 = create_fresh_var_name () in
    let b = create_fresh_var_name () in
    let q = create_fresh_var_name () in
    let loc = Ppxlib.Location.none in
    let fst = if is_or then [%expr !!true] else [%expr !!false] in
    let snd = if is_or then [%expr !!false] else [%expr !!true] in
    [%expr
      fun [%p create_pat a1] [%p create_pat a2] [%p create_logic_var q] ->
        call_fresh (fun [%p create_pat b] ->
          [%e create_id a1] [%e create_id b]
          &&& conde
                [ [%e create_id b] === [%e fst] &&& ([%e create_id q] === [%e fst])
                ; [%e create_id b] === [%e snd] &&& [%e create_id a2] [%e create_id q]
                ])]
  and translate_bool_funs expr is_or =
    if has_named_attribute "rel" expr.exp_attributes
    then translate_bool_funs_without_false is_or
    else translate_bool_funs_with_false is_or
  and translate_eq_funs is_eq =
    let a1 = create_fresh_var_name () in
    let a2 = create_fresh_var_name () in
    let b1 = create_fresh_var_name () in
    let b2 = create_fresh_var_name () in
    let q = create_fresh_var_name () in
    let loc = Ppxlib.Location.none in
    let fst = if is_eq then [%expr !!true] else [%expr !!false] in
    let snd = if is_eq then [%expr !!false] else [%expr !!true] in
    [%expr
      fun [%p create_pat a1] [%p create_pat a2] [%p create_logic_var q] ->
        call_fresh (fun [%p create_pat b1] ->
          call_fresh (fun [%p create_pat b2] ->
            [%e create_id a1] [%e create_id b1]
            &&& [%e create_id a2] [%e create_id b2]
            &&& conde
                  [ [%e create_id b1]
                    === [%e create_id b2]
                    &&& ([%e create_id q] === [%e fst])
                  ; [%e create_id b1]
                    =/= [%e create_id b2]
                    &&& ([%e create_id q] === [%e snd])
                  ]))]
  and translate_not_fun () =
    let a = create_fresh_var_name () in
    let b = create_fresh_var_name () in
    let q = create_fresh_var_name () in
    let loc = Ppxlib.Location.none in
    [%expr
      fun [%p create_pat a] [%p create_logic_var q] ->
        call_fresh (fun [%p create_pat b] ->
          [%e create_id a] [%e create_id b]
          &&& conde
                [ [%e create_id b] === !!true &&& ([%e create_id q] === !!false)
                ; [%e create_id b] === !!false &&& ([%e create_id q] === !!true)
                ])]
  and translate_if cond th el =
    let b = create_fresh_var_name () in
    let q = create_fresh_var_name () in
    let loc = Ppxlib.Location.none in
    [%expr
      fun [%p create_logic_var q] ->
        call_fresh (fun [%p create_pat b] ->
          [%e translate_expression cond] [%e create_id b]
          &&& conde
                [ [%e create_id b]
                  === !!true
                  &&& [%e translate_expression th] [%e create_id q]
                ; [%e create_id b]
                  === !!false
                  &&& [%e translate_expression el] [%e create_id q]
                ])]
  and translate_ident exp txt =
    match txt with
    | Lident "&&" -> translate_bool_funs exp false
    | Lident "||" -> translate_bool_funs exp true
    | Lident "not" -> translate_not_fun ()
    | Lident "=" -> translate_eq_funs true
    | Lident "<>" -> translate_eq_funs false
    | Lident "failwith" ->
      let loc = Ppxlib.Location.none in
      [%expr fun _ _ -> failure]
    | _ -> txt |> mknoloc |> add_translated_module_name_in_ident |> Exp.ident
  and translate_abstraciton case =
    let rec normalize_abstraction expr acc =
      match expr.exp_desc with
      | Texp_function { cases = [ case ] } when pat_is_var case.c_lhs ->
        normalize_abstraction case.c_rhs (case.c_lhs :: acc)
      | _ -> expr, List.rev acc
    in
    let eta_extension expr =
      let rec get_arg_types (typ : Types.type_expr) =
        match Types.get_desc typ with
        | Tarrow (_, l, r, _) -> l :: get_arg_types r
        | Tlink typ -> get_arg_types typ
        | _ -> []
      in
      let arg_types = get_arg_types expr.exp_type in
      List.map (fun t -> create_fresh_var_name (), t) arg_types
    in
    let two_or_more_mentions tactic var_name expr =
      let rec two_or_more_mentions expr count =
        let eval_if_need c e = if c <= 1 then two_or_more_mentions e c else c in
        let rec get_pat_vars : type a. a Typedtree.general_pattern -> _ =
         fun p ->
          match p.pat_desc with
          | Tpat_any | Tpat_constant _ -> []
          | Tpat_var (n, _) -> [ name n ]
          | Tpat_tuple pats -> List.concat_map get_pat_vars pats
          | Tpat_construct (_, _, pats, _) -> List.concat_map get_pat_vars pats
          | Tpat_record (l, _) -> List.concat_map (fun (_, _, p) -> get_pat_vars p) l
          | Tpat_alias (t, n, _) -> name n :: get_pat_vars t
          | Tpat_value x -> get_pat_vars (x :> Typedtree.pattern)
          | Tpat_or (a, b, _) -> get_pat_vars a @ get_pat_vars b
          | Tpat_lazy _ | Tpat_array _ | Tpat_exception _ | Tpat_variant _ ->
            fail_loc expr.exp_loc "not implemented (get_pat_vars)"
        in
        match expr.exp_desc with
        | Texp_constant _ -> count
        | Texp_tuple args | Texp_construct (_, _, args) ->
          List.fold_left eval_if_need count args
        | Texp_ident (_, { txt = Longident.Lident name }, _) ->
          if var_name = name then count + 1 else count
        | Texp_ident _ -> count
        | Texp_function { cases } ->
          let cases =
            List.filter
              (fun c -> List.for_all (( <> ) var_name) @@ get_pat_vars c.c_lhs)
              cases
          in
          let exprs = List.map (fun c -> c.c_rhs) cases in
          if tactic = Nondet
          then List.fold_left eval_if_need count @@ exprs
          else List.fold_left max count @@ List.map (eval_if_need count) exprs
        | Texp_apply (func, args) ->
          let args =
            List.map
              (function
               | _, Some a -> a
               | _, None -> failwith "Not implemented")
              args
          in
          List.fold_left eval_if_need count @@ (func :: args)
        | Texp_ifthenelse (cond, th, Some el) ->
          if tactic = Nondet
          then List.fold_left eval_if_need count [ cond; th; el ]
          else (
            let c1 = eval_if_need count cond in
            max (eval_if_need c1 th) (eval_if_need c1 el))
        | Texp_let (_, bindings, expr) ->
          let bindings =
            List.filter (fun b -> var_name <> get_pat_name b.vb_pat) bindings
          in
          let exprs = expr :: List.map (fun b -> b.vb_expr) bindings in
          List.fold_left eval_if_need count exprs
        | Texp_match (e, cs, _) ->
          let cases =
            List.filter
              (fun c -> List.for_all (( <> ) var_name) @@ get_pat_vars c.c_lhs)
              cs
          in
          let exprs = List.map (fun c -> c.c_rhs) cases in
          if tactic = Nondet
          then List.fold_left eval_if_need count @@ (e :: exprs)
          else (
            let c1 = eval_if_need count e in
            List.fold_left max c1 @@ List.map (eval_if_need c1) exprs)
        | Texp_record { fields; extended_expression } ->
          let c' =
            match extended_expression with
            | None -> count
            | Some e -> eval_if_need count e
          in
          Array.fold_left
            (fun c (_, ld) ->
              match ld with
              | Overridden (_, e) -> eval_if_need c e
              | _ -> c)
            c'
            fields
        | Texp_field (e, _, _) -> eval_if_need count e
        | Texp_open (_, e) -> two_or_more_mentions e count
        | Texp_letop { let_; body } ->
          let count = two_or_more_mentions body.c_rhs count in
          if List.exists (( = ) var_name) @@ get_pat_vars body.c_lhs
          then count
          else eval_if_need count let_.bop_exp
        | Texp_unreachable
        | Texp_try (_, _)
        | Texp_variant (_, _)
        | Texp_setfield (_, _, _, _)
        | Texp_array _
        | Texp_sequence (_, _)
        | Texp_while (_, _)
        | Texp_for (_, _, _, _, _, _)
        | Texp_new (_, _, _)
        | Texp_instvar (_, _, _)
        | Texp_setinstvar (_, _, _, _)
        | Texp_override (_, _)
        | Texp_letmodule (_, _, _, _, _)
        | Texp_letexception (_, _)
        | Texp_assert _ | Texp_lazy _
        | Texp_object (_, _)
        | Texp_pack _
        | Texp_extension_constructor (_, _)
        | Texp_ifthenelse (_, _, None)
        | _ ->
          Location.raise_errorf
            "Not implemented expr: %a"
            Pprintast.expression
            (Untypeast.untype_expression expr)
      in
      two_or_more_mentions expr 0 >= 2
    in
    let need_to_activate p e =
      is_primary_type p.pat_type
      && (is_active_arg p
          || (params.high_order_paprams.activate_tactic <> Off
              && two_or_more_mentions
                   params.high_order_paprams.activate_tactic
                   (get_pat_name p)
                   e))
    in
    let create_simple_arg var =
      let fresh_n = create_fresh_var_name () in
      let loc = Ppxlib.Location.none in
      create_fun fresh_n [%expr [%e create_id fresh_n] === [%e create_id var]]
    in
    let body, real_vars = normalize_abstraction case.c_rhs [ case.c_lhs ] in
    let eta_vars = eta_extension body in
    let translated_body = translate_expression body in
    let result_var = create_fresh_var_name () in
    let body_with_eta_args =
      create_apply translated_body
      @@ List.map create_id
      @@ List.map fst eta_vars
      @ [ result_var ]
    in
    let active_vars =
      List.map get_pat_name @@ List.filter (fun p -> need_to_activate p body) real_vars
    in
    let fresh_vars = List.map (fun _ -> create_fresh_var_name ()) active_vars in
    let abstr_body = List.fold_right create_fun active_vars body_with_eta_args in
    let body_with_args =
      create_apply abstr_body @@ List.map create_simple_arg fresh_vars
    in
    let conjs =
      List.map2
        (fun a b -> create_apply (create_id a) @@ [ create_id b ])
        active_vars
        fresh_vars
    in
    let full_conj = create_conj (conjs @ [ body_with_args ]) in
    let with_fresh = List.fold_right create_fresh fresh_vars full_conj in
    let first_fun = create_fun result_var with_fresh in
    let with_eta = List.fold_right create_fun (List.map fst eta_vars) first_fun in
    List.fold_right create_fun (List.map get_pat_name real_vars) with_eta
  and translate_apply f a l =
    create_apply
      (translate_expression f)
      (List.map
         (function
          | _, Some e ->
            if is_primary_type e.exp_type
            then mark_fo_arg @@ translate_expression e
            else translate_expression e
          | _ -> fail_loc l "Incorrect argument")
         a)
  and translate_match_without_scrutinee
    loc
    (cases : 'a Typedtree.case list)
    (typ_desc : Types.type_desc)
    =
    match typ_desc with
    | Tarrow (_, _, r, _) ->
      let new_scrutinee = create_fresh_var_name () in
      let translated_match = translate_match loc (create_id new_scrutinee) [] cases r in
      create_fun new_scrutinee translated_match
    | Tlink typ -> translate_match_without_scrutinee loc cases typ_desc
    | _ -> fail_loc loc "Incorrect type for 'function'"
  and translate_match_with_scrutinee
        : 'a.
          loc
          -> Typedtree.expression
          -> 'a Typedtree.case list
          -> Types.type_expr
          -> Parsetree.expression
    =
    fun (type a) loc s (cases : a Typedtree.case list) typ ->
     translate_match loc (translate_expression s) s.exp_attributes cases typ
  and translate_match
        : 'a.
          loc
          -> Parsetree.expression
          -> attributes
          -> 'a Typedtree.case list
          -> Types.type_expr
          -> Parsetree.expression
    =
    fun (type a) loc translated_scrutinee attrs (cases : a Typedtree.case list) typ ->
     let is_disj_pats = cases |> List.map (fun c -> c.c_lhs) |> is_disj_pats in
     if params.use_wildcard || is_disj_pats
     then (
       let high_extra_args = create_fresh_argument_names_by_args typ in
       let result_arg = create_fresh_var_name () in
       let extra_args = high_extra_args @ [ result_arg ] in
       let scrutinee_var = create_fresh_var_name () in
       let create_subst v =
         let abs_v = create_fresh_var_name () in
         let unify = [%expr [%e create_id v] === [%e create_id abs_v]] in
         create_fun abs_v unify
       in
       let translate_match_pat (pat, als) =
         let unify = [%expr [%e create_id scrutinee_var] === [%e pat]] in
         let unifies = List.map alias2unify als in
         create_conj (unify :: unifies)
       in
       let translate_case case prevs =
         let prevs, wilds =
           prevs
           |> List.map (fun p -> translate_or_pats p create_fresh_var_name)
           |> List.split
         in
         let prevs, _ = List.split @@ List.concat prevs in
         let wilds = List.concat wilds in
         let diseqs =
           List.map (fun p -> [%expr [%e create_id scrutinee_var] =/= [%e p]]) prevs
         in
         let p_with_als, vs = translate_or_pats case.c_lhs create_fresh_var_name in
         let pats = create_disj (List.map translate_match_pat p_with_als) in
         let pats_with_diseqs = create_conj (pats :: diseqs) in
         let body =
           create_apply (translate_expression case.c_rhs) (List.map create_id extra_args)
         in
         let abst_body = List.fold_right create_fun vs body in
         let subst = List.map create_subst vs in
         let total_body = create_apply abst_body subst in
         let conj = create_conj [ pats_with_diseqs; total_body ] in
         conj |> List.fold_right create_wildcard wilds |> List.fold_right create_fresh vs
       in
       let new_cases =
         if is_disj_pats
         then List.map (fun c -> translate_case c []) cases
         else
           List.rev
           @@ fst
           @@ List.fold_left
                (fun (cs, prevs) case ->
                  ( translate_case case prevs :: cs
                  , remove_aliases_from_pat case.c_lhs :: prevs ))
                ([], [])
                cases
       in
       let disj = create_disj new_cases in
       let scrutinee = create_apply translated_scrutinee [ create_id scrutinee_var ] in
       let scrutinee = { scrutinee with pexp_attributes = attrs } in
       let conj = create_conj [ scrutinee; disj ] in
       let fresh = create_fresh scrutinee_var conj in
       let with_res = [%expr fun [%p create_logic_var result_arg] -> [%e fresh]] in
       List.fold_right create_fun high_extra_args with_res)
     else fail_loc loc "Pattern matching contains unified patterns"
  and translate_bind bind =
    let bind = { bind with vb_pat = normalize_let_name bind.vb_pat } in
    let rec get_tabling_rank (typ : Types.type_expr) =
      let loc = Ppxlib.Location.none in
      match Types.get_desc typ with
      | Tarrow (_, _, right_typ, _) ->
        create_apply [%expr Tabling.succ] [ get_tabling_rank right_typ ]
      | Tlink typ -> get_tabling_rank typ
      | _ -> [%expr Tabling.one]
    in
    let body = bind.vb_expr in
    let new_body = translate_expression body in
    let typ = body.exp_type in
    let has_tabled_attr =
      List.exists (fun a -> a.attr_name.txt = tabling_attr_name) bind.vb_attributes
    in
    let tabled_body =
      if (not has_tabled_attr) || has_func_arg typ
      then new_body
      else (
        let name = get_pat_name bind.vb_pat in
        let unrec_body = create_fun name new_body in
        let recfunc_argument_name = create_fresh_var_name () in
        let recfunc_argument = create_id recfunc_argument_name in
        let argument_names1 = create_fresh_argument_names_by_type typ in
        let arguments1 = List.map create_id argument_names1 in
        let res_arg_name_1 = create_fresh_var_name () in
        let rec_arg_1 = create_id res_arg_name_1 in
        let argument_names2 = create_fresh_argument_names_by_type typ in
        let arguments2 = List.map create_id argument_names2 in
        let recfunc_with_args =
          create_apply recfunc_argument (List.append arguments2 [ rec_arg_1 ])
        in
        let conjuncts1 =
          List.map2 (fun q1 q2 -> create_apply q1 [ q2 ]) arguments1 arguments2
        in
        let conjs_and_recfunc =
          create_conj @@ List.append conjuncts1 [ recfunc_with_args ]
        in
        let freshing_and_recfunc =
          List.fold_right create_fresh argument_names2 conjs_and_recfunc
        in
        let lambdas_and_recfunc =
          List.fold_right
            create_fun
            (List.append argument_names1 [ res_arg_name_1 ])
            freshing_and_recfunc
        in
        let argument_names3 = create_fresh_argument_names_by_type typ in
        let arguments3 = List.map create_id argument_names3 in
        let argument_names4 = create_fresh_argument_names_by_type typ in
        let arguments4 = List.map create_id argument_names4 in
        let loc = Ppxlib.Location.none in
        let unified_vars1 =
          List.map2 (fun a b -> [%expr [%e a] === [%e b]]) arguments3 arguments4
        in
        let lambda_vars1 = List.map2 create_fun argument_names3 unified_vars1 in
        let new_nody = create_apply unrec_body (lambdas_and_recfunc :: lambda_vars1) in
        let lambda_new_body =
          List.fold_right create_fun (recfunc_argument_name :: argument_names4) new_nody
        in
        let abling_rank = get_tabling_rank typ in
        let tabled_body =
          create_apply [%expr Tabling.tabledrec] [ abling_rank; lambda_new_body ]
        in
        let argument_names5 = create_fresh_argument_names_by_type typ in
        let arguments5 = List.map create_id argument_names5 in
        let res_arg_name_5 = create_fresh_var_name () in
        let rec_arg_5 = create_id res_arg_name_5 in
        let argument_names6 = create_fresh_argument_names_by_type typ in
        let arguments6 = List.map create_id argument_names6 in
        let tabled_body_with_args =
          create_apply tabled_body (List.append arguments6 [ rec_arg_5 ])
        in
        let conjuncts2 =
          List.map2 (fun q1 q2 -> create_apply q1 [ q2 ]) arguments5 arguments6
        in
        let conjs_and_tabled =
          create_conj (List.append conjuncts2 [ tabled_body_with_args ])
        in
        let freshing_and_tabled =
          List.fold_right create_fresh argument_names6 conjs_and_tabled
        in
        let lambdas_and_tabled =
          List.fold_right
            create_fun
            (List.append argument_names5 [ res_arg_name_5 ])
            freshing_and_tabled
        in
        lambdas_and_tabled)
    in
    let new_name = get_pat_name bind.vb_pat in
    let nvb = Vb.mk (create_pat new_name) tabled_body in
    ( new_name
    , if is_primary_type bind.vb_expr.exp_type
      then
        { nvb with pvb_attributes = [ Attr.mk (mknoloc "need_CbN") (Parsetree.PStr []) ] }
      else nvb )
  and translate_let flag binds expr =
    let nvbs = List.map (fun b -> snd @@ translate_bind b) binds in
    Exp.let_ flag nvbs (translate_expression expr)
  and translate_new_record fields typ loc =
    let fields = Array.to_list fields in
    let ctor = ctor_for_record loc typ in
    let mvar = create_fresh_var_name () in
    let vars = List.map (fun _ -> create_fresh_var_name ()) fields in
    let exprs =
      List.map
        (function
         | _, Overridden (_, expr) -> translate_expression expr
         | _, Kept _ -> failwith "not implemented")
        fields
    in
    let calls = List.map2 (fun e v -> create_apply e [ create_id v ]) exprs vars in
    let uni =
      [%expr [%e create_id mvar] === [%e create_apply ctor (List.map create_id vars)]]
    in
    let conj = create_conj (uni :: calls) in
    let with_fr = List.fold_right create_fresh vars conj in
    create_fun mvar with_fr
  and translate_extended_record expr fields typ loc =
    let fields = Array.to_list fields in
    let ctor = ctor_for_record loc typ in
    let mvar = create_fresh_var_name () in
    let vars = List.map (fun _ -> create_fresh_var_name ()) fields in
    let bas_call =
      create_apply
        (translate_expression expr)
        [ create_apply ctor (List.map create_id vars) ]
    in
    let exprs =
      fields
      |> List.map (function
           | _, Overridden (_, e) ->
             Some (translate_expression e, create_fresh_var_name ())
           | _ -> None)
    in
    let calls =
      exprs
      |> List.filter_map (function
           | Some (e, v) -> Some (create_apply e [ create_id v ])
           | None -> None)
    in
    let real_vs =
      List.map2
        (fun e v ->
          match e, v with
          | Some (_, v), _ -> v
          | None, v -> v)
        exprs
        vars
    in
    let add_vs =
      exprs
      |> List.filter_map (function
           | Some (e, v) -> Some v
           | None -> None)
    in
    let uni =
      [%expr [%e create_id mvar] === [%e create_apply ctor (List.map create_id real_vs)]]
    in
    let conj = create_conj (uni :: bas_call :: calls) in
    let with_fr = List.fold_right create_fresh (vars @ add_vs) conj in
    create_fun mvar with_fr
  and translate_field expr field (field_info : Types.label_description) loc =
    let ctor = ctor_for_record loc expr.exp_type in
    let mvar = create_fresh_var_name () in
    let fields = Array.to_list field_info.lbl_all in
    let index = field_info.lbl_pos in
    let all_vars =
      fields
      |> List.mapi (fun i _ -> if i == index then mvar else create_fresh_var_name ())
    in
    let without_mvar = all_vars |> filteri (fun i _ -> i <> index) in
    let arg = create_apply ctor (List.map create_id all_vars) in
    let call = create_apply (translate_expression expr) [ arg ] in
    let with_fr = List.fold_right create_fresh without_mvar call in
    create_fun mvar with_fr
  and translate_let_star loc let_ body =
    if let_.bop_op_name.txt = source_bind_name
    then (
      let fun_typ =
        Types.Tarrow
          (Asttypes.Nolabel, body.c_lhs.pat_type, body.c_rhs.exp_type, Types.commu_var ())
      in
      let translated_fun = translate_match_without_scrutinee loc [ body ] fun_typ in
      let body = translate_expression let_.bop_exp in
      create_apply (create_id @@ bind_name) [ body; translated_fun ])
    else fail_loc loc "Unexpected let operation (only 'let*' is supported)"
  and translate_rel_memo e =
    let result_arg = create_fresh_var_name () in
    let rel_exp = mark_memo_expr @@ Untype_more.(skip_bindings.expr skip_bindings) e in
    let loc = Ppxlib.Location.none in
    [%expr
      fun [%p create_logic_var result_arg] ->
        [%e create_id result_arg] === !!true &&& [%e rel_exp]]
  and translate_expression e =
    match e.exp_desc with
    | Texp_apply
        ( { exp_desc = Texp_ident (_, { txt = Lident "memo" }, _) }
        , [ (Nolabel, Some arg) ] ) -> translate_rel_memo arg
    | Texp_constant _ -> translate_construct e
    | Texp_construct _ -> translate_construct e
    | Texp_tuple _ -> translate_construct e
    | Texp_ident (_, { txt }, _) -> translate_ident e txt
    | Texp_function { cases = [ c ] } when pat_is_var c.c_lhs -> translate_abstraciton c
    | Texp_function { cases } ->
      translate_match_without_scrutinee e.exp_loc cases @@ Types.get_desc e.exp_type
    | Texp_apply (f, a) -> translate_apply f a e.exp_loc
    | Texp_match (s, cs, _) -> translate_match_with_scrutinee e.exp_loc s cs e.exp_type
    | Texp_ifthenelse (cond, th, Some el) -> translate_if cond th el
    | Texp_let (flag, binds, expr) -> translate_let flag binds expr
    | Texp_record { fields; extended_expression = None } ->
      translate_new_record fields e.exp_type e.exp_loc
    | Texp_record { fields; extended_expression = Some a } ->
      translate_extended_record a fields e.exp_type e.exp_loc
    | Texp_field (expr, field, field_info) ->
      translate_field expr field field_info e.exp_loc
    | Texp_letop { let_; body } -> translate_let_star e.exp_loc let_ body
    | _ -> fail_loc e.exp_loc "Incorrect expression"
  in
  let translate_sign ~loc attributes sign =
    let open Parsetree in
    if has_named_attribute "same_in_ocanren" attributes
    then sign
    else (
      let translated_sign =
        List.map
          (fun sign_item ->
            match sign_item.psig_desc with
            | Psig_value vd ->
              { sign_item with
                psig_desc = Psig_value { vd with pval_type = translate_type vd.pval_type }
              }
            | Psig_type (_, tdecls)
              when List.for_all (fun tdecl -> tdecl.ptype_kind = Ptype_abstract) tdecls ->
              sign_item
            | _ -> fail_loc loc "Only values and abstract in signatures are supported")
          sign
      in
      [ Sig.module_
        @@ Md.mk (mknoloc @@ Some translated_module_name)
        @@ Mty.signature translated_sign
      ])
  in
  let rec translate_structure_item i =
    let loc = i.str_loc in
    match i.str_desc with
    | Tstr_modtype
        { mtd_type = Some { mty_type = Mty_signature sign }; mtd_name; mtd_attributes } ->
      { translated =
          [ Ast_helper.(
              Str.modtype ~loc
              @@ Mtd.mk
                   ~loc
                   mtd_name
                   ~typ:
                     (Mty.signature
                      @@ translate_sign ~loc mtd_attributes
                      @@ Untype_more.untype_types_sign sign))
          ]
      ; synonyms = []
      ; ocaml_code = []
      }
    | Tstr_module { mb_name; mb_expr = { mod_desc; _ } } ->
      let param, stru =
        match mod_desc with
        | Tmod_functor ((Named _ as param), { mod_desc = Tmod_structure stru }) ->
          Some (Untype_more.untype_functor_param param), stru
        | Tmod_structure stru -> None, stru
        | _ -> fail_loc loc "Unexpected kind of module"
      in
      let translated_param =
        Option.map
          (function
           | Unit -> Unit
           | Named (n, ({ pmty_desc = Pmty_signature sign; pmty_attributes } as mt)) ->
             Named
               ( n
               , { mt with
                   pmty_desc = Pmty_signature (translate_sign ~loc pmty_attributes sign)
                 } )
           | Named (name, { pmty_desc = Pmty_ident _ }) as ident -> ident
           | _ -> fail_loc loc "Unexpected module parameter")
          param
      in
      let { translated; synonyms; ocaml_code } =
        split_translated_structure_item
        @@ List.map translate_structure_item stru.str_items
      in
      let name =
        match mb_name.txt with
        | Some n -> Lident n
        | None -> fail_loc mb_name.loc "Modules without names aren't supported"
      in
      let mk_module name items =
        match translated_param with
        | Some param ->
          Ast_helper.(
            Str.module_ @@ Mb.mk name (Mod.functor_ param @@ Mod.structure items))
        | None -> Ast_helper.(Str.module_ @@ Mb.mk name @@ Mod.structure items)
      in
      let synonyms = create_external_open ~loc name param :: synonyms in
      { translated = [ mk_module mb_name translated ]
      ; synonyms = [ mk_module mb_name synonyms ]
      ; ocaml_code = []
      }
    | Tstr_value (_, [ { vb_attributes } ])
      when has_named_attribute "only_lozovml" vb_attributes ->
      { translated = []; synonyms = []; ocaml_code = [] }
    | Tstr_value (_, [ { vb_pat = { pat_desc = Tpat_var (_, { txt = "memo" }) } } ]) ->
      { translated = []; synonyms = []; ocaml_code = [] }
    | Tstr_value (rec_flag, binds) ->
      let helper bind =
        let name = get_pat_name @@ bind.vb_pat in
        let tr_name, internal_vb = translate_bind bind in
        let open Synonyms_synthesis in
        let interface_vb =
          [ Vb.mk
              (create_pat name)
              (compress create_fresh_var_name tr_name bind.vb_expr.exp_type)
          ]
        in
        internal_vb, Str.value Nonrecursive interface_vb
      in
      let new_binds, synonyms = List.map helper binds |> List.split in
      { translated = [ Str.value rec_flag new_binds ]; synonyms; ocaml_code = [] }
    | Tstr_type (rec_flag, decls) ->
      let new_decls = List.map mark_type_declaration decls in
      { translated =
          [ untyper.structure_item
              untyper
              { i with str_desc = Tstr_type (rec_flag, new_decls) }
          ]
      ; synonyms = []
      ; ocaml_code = []
      }
    | Tstr_open od ->
      let open_ = untyper.open_declaration untyper od in
      let open_ = add_translated_module_name_in_open open_ in
      let open_ = Str.open_ open_ in
      { translated = [ open_ ]; synonyms = [ open_ ]; ocaml_code = [] }
    | Tstr_include { incl_mod = { mod_desc = Tmod_structure stru } } ->
      split_translated_structure_item @@ List.map translate_structure_item stru.str_items
    | Tstr_attribute
        { attr_name = { txt = "only_ocanren" }; attr_payload = Parsetree.PStr stru } ->
      { translated = stru; synonyms = []; ocaml_code = [] }
    | Tstr_attribute { attr_name = { txt = "ocaml" }; attr_payload = Parsetree.PStr stru }
      -> { translated = []; synonyms = []; ocaml_code = stru }
    | _ -> fail_loc loc "Incorrect structure item"
  in
  let translate_structure t =
    let mk_module name items =
      Ast_helper.(Str.module_ @@ Mb.mk name @@ Mod.structure items)
    in
    let { translated; synonyms; ocaml_code } =
      split_translated_structure_item @@ List.map translate_structure_item t.str_items
    in
    let opens, rest_synonyms =
      List.partition
        (fun si ->
          match si.pstr_desc with
          | Pstr_open _ -> true
          | _ -> false)
        synonyms
    in
    let open_main_module =
      create_external_open ~loc:Location.none (Lident translated_module_name) None
    in
    let synonyms = opens @ (open_main_module :: rest_synonyms) in
    { translated = [ mk_module (mknoloc (Some translated_module_name)) translated ]
    ; synonyms = [ mk_module (mknoloc (Some synonoms_module_name)) synonyms ]
    ; ocaml_code
    }
  in
  translate_structure tast
;;

(*****************************************************************************************************************************)

let only_generate tast params =
  try
    let start_index = get_max_index tast in
    let reductor = Beta_reductor.beta_reductor start_index params.subst_only_util_vars in
    let { translated; synonyms; ocaml_code } = translate_high tast start_index params in
    let translated =
      translated
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
    in
    (create_external_attribute "ocaml.warning" "-8" :: Untypeast.untype_structure tast)
    @ (create_external_attribute "ocaml.warning" "+8" :: translated)
    @ synonyms
    @ ocaml_code
  with
  | TranslatorError e as exc ->
    report_error Format.std_formatter e;
    raise exc
;;
