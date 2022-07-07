open Parsetree
open Ast_helper
open Util

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