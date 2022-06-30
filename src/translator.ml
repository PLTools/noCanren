open Longident
open Typedtree
open Ast_helper
open Ident
open Parsetree
open Util
open Translator_unnesting

let () = Printexc.record_backtrace true

(*****************************************************************************************************************************)

let translate_high tast start_index params =
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
    | Texp_constant c -> create_inj (Exp.constant (Untypeast.constant c)), [], []
    | Texp_construct ({ txt = Lident s }, _, []) when s = "true" || s = "false" ->
      create_inj (untyper.expr untyper e), [], []
    | Texp_construct ({ txt = Lident s }, _, []) when s = "Nothing" ->
      let new_var = create_fresh_var_name () in
      create_id new_var, [ FailureExpr ], [ new_var ]
    | Texp_construct ({ txt = Lident s }, _, [ a ]) when s = "Just" -> unnest_constuct a
    | Texp_tuple l ->
      let new_args, als, vars = List.map unnest_constuct l |> split3 in
      ( fold_right1
          (fun e1 e2 -> create_apply (mark_constr [%expr pair]) [ e1; e2 ])
          new_args
      , List.concat als
      , List.concat vars )
    | Texp_construct (name, desc, args) ->
      let args = get_constr_args loc desc args in
      let new_args, als, vars = List.map unnest_constuct args |> split3 in
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
      ( create_apply (mknoloc new_name |> Exp.ident |> mark_constr) new_args
      , List.concat als
      , List.concat vars )
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
        [%e op] ([%e create_id a1] !!true) ([%e create_id a2] !!true)]
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
    | _ -> id2id_o txt |> mknoloc |> Exp.ident
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
          if get_pat_name @@ body.c_lhs == var_name
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
      List.map (fun p -> get_pat_name p ^ "_o")
      @@ List.filter (fun p -> need_to_activate p body) real_vars
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
    List.fold_right
      create_fun
      (List.map (fun p -> get_pat_name p ^ "_o") real_vars)
      with_eta
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
      (typ : Types.type_expr)
    =
    match Types.get_desc typ with
    | Tarrow (_, _, r, _) ->
      let new_scrutinee = create_fresh_var_name () in
      let translated_match = translate_match loc (create_id new_scrutinee) [] cases r in
      create_fun new_scrutinee translated_match
    | Tlink typ -> translate_match_without_scrutinee loc cases typ
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
     if cases |> List.map (fun c -> c.c_lhs) |> is_disj_pats
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
       let translate_case case =
         let p_with_als, vs = translate_or_pats case.c_lhs create_fresh_var_name in
         let pats = create_disj (List.map translate_match_pat p_with_als) in
         let body =
           create_apply (translate_expression case.c_rhs) (List.map create_id extra_args)
         in
         let abst_body =
           List.fold_right create_fun (List.map (fun v -> v ^ "_o") vs) body
         in
         let subst = List.map create_subst vs in
         let total_body = create_apply abst_body subst in
         let conj = create_conj [ pats; total_body ] in
         List.fold_right create_fresh vs conj
       in
       let new_cases = List.map translate_case cases in
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
    let rec is_func_type (t : Types.type_expr) =
      match Types.get_desc t with
      | Tarrow _ -> true
      | Tlink t' -> is_func_type t'
      | _ -> false
    in
    let rec has_func_arg (t : Types.type_expr) =
      match Types.get_desc t with
      | Tarrow (_, f, s, _) -> is_func_type f || has_func_arg s
      | Tlink t' -> has_func_arg t'
      | _ -> false
    in
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
        let name = get_pat_name bind.vb_pat ^ "_o" in
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
    let new_name = create_pat (infix_to_prefix (get_pat_name bind.vb_pat) ^ "_o") in
    let nvb = Vb.mk new_name tabled_body in
    if is_primary_type bind.vb_expr.exp_type
    then
      { nvb with pvb_attributes = [ Attr.mk (mknoloc "need_CbN") (Parsetree.PStr []) ] }
    else nvb
  and translate_let flag bind expr =
    let nvb = translate_bind bind in
    Exp.let_ flag [ nvb ] (translate_expression expr)
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
      let var = (get_pat_name @@ body.c_lhs) ^ "_o" in
      let exp_in = translate_expression body.c_rhs in
      let body = translate_expression let_.bop_exp in
      create_apply (create_id @@ bind_name ^ "_o") [ body; create_fun var exp_in ])
    else fail_loc loc "Unexpected let operation (only 'let*' is supported)"
  and translate_rel_memo e =
    let result_arg = create_fresh_var_name () in
    let rel_exp = Untype_more.(skip_bindings.expr skip_bindings) e in
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
      translate_match_without_scrutinee e.exp_loc cases e.exp_type
    | Texp_apply (f, a) -> translate_apply f a e.exp_loc
    | Texp_match (s, cs, _) -> translate_match_with_scrutinee e.exp_loc s cs e.exp_type
    | Texp_ifthenelse (cond, th, Some el) -> translate_if cond th el
    | Texp_let (flag, [ bind ], expr) -> translate_let flag bind expr
    | Texp_record { fields; extended_expression = None } ->
      translate_new_record fields e.exp_type e.exp_loc
    | Texp_record { fields; extended_expression = Some a } ->
      translate_extended_record a fields e.exp_type e.exp_loc
    | Texp_field (expr, field, field_info) ->
      translate_field expr field field_info e.exp_loc
    | Texp_letop { let_; body } -> translate_let_star e.exp_loc let_ body
    | _ -> fail_loc e.exp_loc "Incorrect expression"
  in
  let rec translate_structure_item i =
    match i.str_desc with
    | Tstr_modtype { mtd_type = Some { mty_type = Mty_signature sign }; mtd_name } ->
      let loc = i.str_loc in
      [ Ast_helper.(
          Str.modtype ~loc:i.str_loc
          @@ Mtd.mk
               ~loc
               mtd_name
               ~typ:(Mty.signature @@ Untype_more.untype_types_sign sign))
      ]
    | Tstr_module
        { mb_name
        ; mb_expr =
            { mod_desc =
                Tmod_functor
                  ((Named (_, lid, _) as param), { mod_desc = Tmod_structure stru })
            }
        } ->
      [ Ast_helper.(
          Str.module_
          @@ Mb.mk
               mb_name
               (Mod.functor_ (Untype_more.untype_functor_param param)
               @@ Mod.structure
               @@ List.concat_map translate_structure_item stru.str_items))
      ]
    | Tstr_value (_, [ { vb_attributes } ])
      when has_named_attribute "only_lozovml" vb_attributes -> []
    | Tstr_value (_, [ { vb_pat = { pat_desc = Tpat_var (_, { txt = "memo" }) } } ]) -> []
    | Tstr_value (rec_flag, [ bind ]) -> [ Str.value rec_flag [ translate_bind bind ] ]
    | Tstr_type (rec_flag, decls) ->
      let new_decls = List.map mark_type_declaration decls in
      [ untyper.structure_item
          untyper
          { i with str_desc = Tstr_type (rec_flag, new_decls) }
      ]
    | Tstr_open _ -> [ untyper.structure_item untyper i ]
    | Tstr_include { incl_mod = { mod_desc = Tmod_structure stru } } ->
      List.concat_map translate_structure_item stru.str_items
    | Tstr_attribute
        { attr_name = { txt = "only_ocanren" }; attr_payload = Parsetree.PStr stru } ->
      stru
    | _ -> fail_loc i.str_loc "Incorrect structure item"
  in
  let translate_structure t = List.concat_map translate_structure_item t.str_items in
  translate_structure tast
;;

(*****************************************************************************************************************************)

let add_packages ast =
  List.map (fun n -> Lident n |> mknoloc |> Mod.ident |> Opn.mk |> Str.open_) packages
  @ ast
;;

(*****************************************************************************************************************************)

let beta_reductor minimal_index only_q =
  let need_subst name arg =
    (not only_q)
    ||
    let arg_is_var =
      match arg.pexp_desc with
      | Pexp_ident _ -> true
      | _ -> false
    in
    let prefix_length = String.length fresh_var_prefix in
    let length = String.length name in
    let index =
      if length > prefix_length && String.sub name 0 prefix_length = fresh_var_prefix
      then (
        try String.sub name prefix_length (length - prefix_length) |> int_of_string with
        | Failure _ -> -1)
      else -1
    in
    index >= minimal_index || arg_is_var
  in
  let name_from_pat pat =
    match pat.ppat_desc with
    | Ppat_var loc -> loc.txt
    | _ -> fail_loc pat.ppat_loc "Incorrect pattern in beta reduction"
  in
  let rec substitute' expr var subst =
    match expr.pexp_desc with
    | Pexp_ident { txt = Lident name } -> if name = var then subst else expr
    | Pexp_fun (_, _, pat, body) ->
      let name = name_from_pat pat in
      let loc = Ppxlib.Location.none in
      if name = var then expr else [%expr fun [%p pat] -> [%e substitute body var subst]]
    | Pexp_apply (func, args) ->
      List.map snd args
      |> List.map (fun a -> substitute a var subst)
      |> create_apply (substitute func var subst)
    | Pexp_let (flag, vbs, expr) ->
      let is_rec = flag = Recursive in
      let var_in_binds =
        List.map (fun vb -> name_from_pat vb.pvb_pat) vbs |> List.exists (( = ) var)
      in
      let subst_in_bind bind =
        if (is_rec && var_in_binds) || ((not is_rec) && var = name_from_pat bind.pvb_pat)
        then bind
        else { bind with pvb_expr = substitute bind.pvb_expr var subst }
      in
      let new_vbs = List.map subst_in_bind vbs in
      Exp.let_ flag new_vbs (if var_in_binds then expr else substitute expr var subst)
    | Pexp_construct (name, Some expr) ->
      Some (substitute expr var subst) |> Exp.construct name
    | Pexp_tuple exprs -> List.map (fun e -> substitute e var subst) exprs |> Exp.tuple
    | _ -> expr
  and substitute expr var subst =
    let res = substitute' expr var subst in
    { res with pexp_attributes = expr.pexp_attributes }
  in
  let rec beta_reduction' expr args =
    match expr.pexp_desc with
    | Pexp_apply (func, args') ->
      let old_args = List.map snd args' in
      let new_args = List.map (fun a -> beta_reduction a []) old_args in
      List.append new_args args |> beta_reduction func
    | Pexp_fun (_, _, pat, body) ->
      let var =
        match pat.ppat_desc with
        | Ppat_var v -> v.txt
        | _ -> fail_loc pat.ppat_loc "Incorrect arg name in beta reduction"
      in
      (match args with
      | arg :: args' when need_subst var arg ->
        beta_reduction (substitute body var arg) args'
      | _ ->
        let loc = Ppxlib.Location.none in
        create_apply [%expr fun [%p pat] -> [%e beta_reduction body []]] args)
    | Pexp_let (flag, vbs, expr) ->
      let new_vbs =
        List.map (fun v -> { v with pvb_expr = beta_reduction v.pvb_expr [] }) vbs
      in
      let new_expr = beta_reduction expr args in
      Exp.let_ flag new_vbs new_expr
    | Pexp_construct (name, Some expr) ->
      Some (beta_reduction expr []) |> Exp.construct name
    | Pexp_tuple args -> List.map (fun a -> beta_reduction a []) args |> Exp.tuple
    | _ -> create_apply expr args
  and beta_reduction expr args =
    let res = beta_reduction' expr args in
    { res with pexp_attributes = expr.pexp_attributes }
  in
  let expr _ x = beta_reduction x [] in
  { Ast_mapper.default_mapper with expr }
;;

(*****************************************************************************************************************************)

let fresh_and_conjs_normalizer params =
  let has_heavy_attr e =
    List.exists (fun a -> a.attr_name.txt = "heavy") e.pexp_attributes
  in
  let is_failure e =
    match e.pexp_desc with
    | Pexp_ident { txt = Lident "failure" } -> true
    | _ -> false
  in
  let rec split_conjs = function
    | [] -> [], [], []
    | c :: cs ->
      let unifies, conjs, heavies = split_conjs cs in
      (match c.pexp_desc with
      | Pexp_apply ({ pexp_desc = Pexp_ident { txt = Lident "===" } }, _) ->
        c :: unifies, conjs, heavies
      | _ when has_heavy_attr c -> unifies, conjs, c :: heavies
      | _ -> unifies, c :: conjs, heavies)
  in
  let rec get_conjs_and_vars expr =
    match expr.pexp_desc with
    | Pexp_apply
        ( { pexp_desc = Pexp_ident { txt = Lident "call_fresh" } }
        , [ (_, { pexp_desc = Pexp_fun (_, _, { ppat_desc = Ppat_var { txt } }, body) }) ]
        ) ->
      let conjs, vars = get_conjs_and_vars body in
      conjs, txt :: vars
    | Pexp_apply ({ pexp_desc = Pexp_ident { txt = Lident "&&&" } }, [ (_, a); (_, b) ])
      ->
      let conjs1, vars1 = get_conjs_and_vars a in
      let conjs2, vars2 = get_conjs_and_vars b in
      conjs1 @ conjs2, vars1 @ vars2
    | Pexp_apply ({ pexp_desc = Pexp_ident { txt = Lident "?&" } }, [ (_, args) ]) ->
      let rec get_conjs_and_vars_from_list args =
        match args.pexp_desc with
        | Pexp_construct ({ txt = Lident "[]" }, _) -> [], []
        | Pexp_construct
            ({ txt = Lident "::" }, Some { pexp_desc = Pexp_tuple [ hd; tl ] }) ->
          let conjs1, vars1 = get_conjs_and_vars hd in
          let conjs2, vars2 = get_conjs_and_vars_from_list tl in
          conjs1 @ conjs2, vars1 @ vars2
        | _ -> fail_loc args.pexp_loc "get_conjs_and_vars_from_list: arg should be list"
      in
      get_conjs_and_vars_from_list args
    | _ -> [ expr ], []
  in
  let normalizer sub expr =
    let conjs, vars = get_conjs_and_vars expr in
    let conjs = List.map (Ast_mapper.default_mapper.expr sub) conjs in
    if List.exists is_failure conjs
    then (
      let loc = Ppxlib.Location.none in
      [%expr failure])
    else (
      let conjs =
        if params.move_unifications
        then (
          let unifies, conjs, heavies = split_conjs conjs in
          unifies @ conjs @ heavies)
        else conjs
      in
      let vars_as_apply = function
        | x :: xs -> create_apply (create_id x) (List.map create_id xs)
        | _ -> failwith "Incorrect variable count"
      in
      let vars_arg = function
        | [ v ] -> Exp.tuple [ create_id v ]
        | _ -> vars_as_apply vars
      in
      if List.length vars > 0
      then (
        let loc = Ppxlib.Location.none in
        if params.syntax_extenstions
        then create_apply [%expr fresh] (vars_arg vars :: conjs)
        else List.fold_right create_fresh vars (create_conj conjs))
      else create_conj conjs)
  in
  { Ast_mapper.default_mapper with expr = normalizer }
;;

(*****************************************************************************************************************************)

type var_type =
  | First
  | High

(* extra argument for tabling *)
let ea4t = "ea4t"

let call_by_need_creator tree =
  let rec has_attr attr = function
    | x :: xs -> x.attr_name.Location.txt = attr || has_attr attr xs
    | [] -> false
  in
  let has_attr_for_expr attr expr = has_attr attr expr.pexp_attributes in
  let is_ctor expr = has_attr_for_expr "it_was_constr" expr in
  let ignored_idents = [ "|||"; "&&&"; "conde"; "==="; "=/="; "!!" ] in
  let rec uniq = function
    | [] -> []
    | x :: xs -> x :: uniq (List.filter (( <> ) x) xs)
  in
  let remove_vars rem l = List.filter (fun x -> List.for_all (( <> ) x) rem) l in
  let get_name_from_pat pat =
    match pat.ppat_desc with
    | Ppat_var x -> x.txt
    | _ -> failwith "Incorrect pattern in call-by-need creator."
  in
  let is_ident expr =
    match expr.pexp_desc with
    | Pexp_ident _ -> true
    | _ -> false
  in
  let is_fresh_ident expr =
    match expr.pexp_desc with
    | Pexp_ident { txt = Lident "fresh" } -> true
    | _ -> false
  in
  let get_name_from_expr expr =
    match expr.pexp_desc with
    | Pexp_ident { txt = Lident n } -> n
    | _ -> failwith "Incorrect variable in expression."
  in
  let convert_fresh_apply_to_vars expr =
    match expr.pexp_desc with
    | Pexp_apply (f, args) ->
      (match f.pexp_desc with
      | Pexp_ident { txt = Lident fst_var } ->
        fst_var :: List.map (fun (_, e) -> get_name_from_expr e) args
      | _ -> failwith "Incorrect the first variable in fresh-expression.")
    | Pexp_tuple [ e ] -> [ get_name_from_expr e ]
    | _ -> failwith "Incorrect list of variables in fresh-expression."
  in
  let rec fv expr =
    match expr.pexp_desc with
    | Pexp_ident { txt = Lident name } ->
      if is_ctor expr || List.exists (( = ) name) ignored_idents then [] else [ name ]
    | Pexp_construct (_, Some expr) -> fv expr
    | Pexp_tuple args -> uniq @@ List.concat @@ List.map fv args
    | Pexp_let (rec_flag, vbs, body) ->
      let exprs = List.map (fun vb -> vb.pvb_expr) vbs in
      let bind_vars = List.map (fun vb -> get_name_from_pat vb.pvb_pat) vbs in
      let fvs = List.map fv exprs in
      let fvs =
        if rec_flag = Recursive then List.map (remove_vars bind_vars) fvs else fvs
      in
      let body_fv = remove_vars bind_vars @@ fv body in
      uniq (body_fv @ List.concat fvs)
    | Pexp_fun (_, _, pat, body) ->
      let bind_var = get_name_from_pat pat in
      let fvs = fv body in
      List.filter (( <> ) bind_var) fvs
    | Pexp_apply (f, args) ->
      let args = List.map snd args in
      if is_fresh_ident f
      then (
        let vars, bodies = List.hd args, List.tl args in
        let vars = convert_fresh_apply_to_vars vars in
        uniq @@ remove_vars vars @@ List.concat @@ List.map fv bodies)
      else uniq (fv f @ List.concat (List.map fv args))
    | _ -> []
  in
  let rec lookup name = function
    | [] -> failwith @@ Printf.sprintf "Environment has no var \"%s\"." name
    | x :: xs ->
      (match x with
      | n, _ -> if n = name then x else lookup name xs)
  in
  let rec split_vars env = function
    | [] -> [], []
    | x :: xs ->
      let f, h = split_vars env xs in
      (match lookup x env with
      | n, First -> n :: f, h
      | n, High -> f, n :: h)
  in
  let vb_is_logic vb = has_attr "need_CbN" vb.pvb_attributes in
  let expr_is_logic e = has_attr "need_CbN" e.pexp_attributes in
  let is_unify_expr e =
    match e.pexp_desc with
    | Pexp_fun (_, _, _, body) ->
      (match body.pexp_desc with
      | Pexp_apply (f, [ _; _ ]) ->
        (match f.pexp_desc with
        | Pexp_ident { txt = Lident n } -> n = "==="
        | _ -> false)
      | _ -> false)
    | _ -> false
  in
  let var_is_logic pat = has_attr "logic" pat.ppat_attributes in
  let rec update_expr env e =
    let loc = Ppxlib.Location.none in
    match e.pexp_desc with
    | Pexp_ident { txt = Lident name } ->
      if is_ctor e || snd (lookup name env) = First then e else [%expr snd [%e e]]
    | Pexp_apply (f, args) ->
      if is_ctor f
      then e
      else (
        let name = get_name_from_expr f in
        if name = "===" || name = "=/=" || name = "!!"
        then e
        else (
          let args = List.map snd args in
          if name = "&&&" || name = "|||" || name = "conde"
          then create_apply f @@ List.map (update_expr env) args
          else if name = "fresh"
          then (
            match args with
            | v :: conjs ->
              let vars = convert_fresh_apply_to_vars v in
              let new_env = List.map (fun x -> x, First) vars @ env in
              let new_conjs = List.map (update_expr new_env) conjs in
              create_apply f (v :: new_conjs)
            | [] -> failwith "not implemented")
          else (
            let need_tabling =
              List.map (fun a -> expr_is_logic a && (not @@ is_unify_expr a)) args
            in
            let new_args = List.map2 (update_arg env) args need_tabling in
            create_apply [%expr snd [%e f]] new_args)))
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
    if is_ident arg
    then arg
    else (
      let vars = fv arg in
      let vars =
        match rec_name with
        | None -> vars
        | Some n -> List.filter (( <> ) n) vars
      in
      let f, h = split_vars env vars in
      let loc = Ppxlib.Location.none in
      let h_part =
        match h with
        | [] -> [%expr []]
        | h :: hs ->
          List.fold_left
            (fun acc h -> [%expr fst [%e create_id h] @ [%e acc]])
            [%expr fst [%e create_id h]]
            hs
      in
      let full =
        List.fold_left
          (fun acc f -> [%expr Obj.magic [%e create_id f] :: [%e acc]])
          h_part
          f
      in
      let new_arg = update_expr env arg in
      let new_arg =
        if need_tabling
        then (
          let abst = create_fun ea4t new_arg in
          let table = [%expr Tabling.tabled Tabling.two [%e abst]] in
          create_apply table [ [%expr List.list [%e create_id ea4t]] ])
        else new_arg
      in
      let tuple = Exp.tuple [ create_id ea4t; new_arg ] in
      [%expr
        let ([%p create_pat ea4t] : (int, int) injected GT.list) = [%e full] in
        [%e tuple]])
  and update_vb is_rec env vb =
    let rec_name = if is_rec then Some (get_name_from_pat vb.pvb_pat) else None in
    { vb with pvb_expr = update_arg ~rec_name env vb.pvb_expr (vb_is_logic vb) }
  and update_vbs env rf vbs =
    let names = List.map (fun vb -> get_name_from_pat vb.pvb_pat) vbs in
    let new_env = List.map (fun n -> n, High) names @ env in
    let is_rec = rf = Recursive in
    let vb_env = if is_rec then new_env else env in
    let new_vbs = List.map (update_vb is_rec vb_env) vbs in
    new_vbs, new_env
  in
  let rec structure env = function
    | [] -> []
    | { pstr_desc = Pstr_value (rf, vbs) } :: xs ->
      let new_vbs, new_env = update_vbs env rf vbs in
      Str.value rf new_vbs :: structure new_env xs
    | x :: xs -> x :: structure env xs
  in
  structure [] tree
;;

let attrs_remover =
  let expr sub expr =
    Ast_mapper.default_mapper.expr sub { expr with pexp_attributes = [] }
  in
  let value_binding sub vb =
    Ast_mapper.default_mapper.value_binding sub { vb with pvb_attributes = [] }
  in
  let pat sub pat = Ast_mapper.default_mapper.pat sub { pat with ppat_attributes = [] } in
  { Ast_mapper.default_mapper with expr; value_binding; pat }
;;

let print_if ppf flag printer arg =
  if !flag then Format.fprintf ppf "%a@." printer arg;
  arg
;;

let eval_if_need flag f = if flag then f else fun x -> x

let only_generate tast params =
  try
    let start_index = get_max_index tast in
    let reductor = beta_reductor start_index params.subst_only_util_vars in
    (if params.unnesting_mode then translate else translate_high) tast start_index params
    |> add_packages
    |> eval_if_need params.beta_reduction (reductor.structure reductor)
    |> eval_if_need
         params.normalization
         (let mapper = fresh_and_conjs_normalizer params in
          mapper.structure mapper)
    |> eval_if_need params.high_order_paprams.use_call_by_need call_by_need_creator
    |> Put_distrib.process params
    |> print_if Format.std_formatter Clflags.dump_parsetree Printast.implementation
    |> print_if Format.std_formatter Clflags.dump_source Pprintast.structure
  with
  | TranslatorError e as exc ->
    report_error Format.std_formatter e;
    raise exc
;;
