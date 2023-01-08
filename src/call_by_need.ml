open Parsetree
open Ast_helper
open Util

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