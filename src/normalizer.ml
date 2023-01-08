open Parsetree
open Ast_helper
open Util

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