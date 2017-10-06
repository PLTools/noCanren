(* Print all fully qualified names in expressions *)
open Printf
open Asttypes
open Longident
open Typedtree
open Tast_mapper

let () = Printexc.record_backtrace true

(* plugin that come with an example *)
let tast_mapper =
  { default with
    expr = (fun mapper expr ->
      match expr with
      | { exp_desc = Texp_ident (path, _lid, _valdesc) }
        ->
        Printf.printf "Ident %S\n%!" (Path.name path);
          expr
      | other -> default.expr mapper other); }

(* registering demo plugin *)
let () =

  (* Typemod.ImplementationHooks.add_hook "ml_to_mk_demo"
    (fun
      (hook_info : Misc.hook_info)
      ((ast, coercion) : Typedtree.structure * Typedtree.module_coercion) ->
        let ast = tast_mapper.structure tast_mapper ast in
        (ast, coercion)
    ); *)

  (* Typemod.InterfaceHooks.add_hook "pptx_example"
    (fun (hook_info : Misc.hook_info)
      (ast : Typedtree.signature) ->

        ast); *)
  ()


module Lozov = struct
open Typedtree
(* open TastGetter *)
open Ident
open Asttypes
open Lexing
open Location

(*****************************************************************************************************************************)

let eq_name          = "="
let true_name        = "true"
let false_name       = "false"

let logic_type_name  = "logic"
let inj_name         = "!!"
let uniq_name        = "==="
let des_constr_name  = "=/="
let or_name          = "|||"
let and_name         = "&&&"
let fresh_name       = "call_fresh"
let fresh_var_prefix = "q"

(*****************************************************************************************************************************)

let packages = ["MiniKanren"]

(*****************************************************************************************************************************)

let rec fold_right0 f l =
  match l with
  | [x]     -> x
  | x :: xs -> fold_right0 f xs |> f x
  | [] -> failwith "bad argument of fold_right0"

(*****************************************************************************************************************************)

let pos         = { pos_fname = ""; pos_lnum = -1; pos_bol = -1; pos_cnum = -1 }
let loc         = { loc_start = pos; loc_end = pos; loc_ghost = true }
let type_expr   = { Types.desc = Types.Tvar None; level = -1; id = -1 }

let create_path name =
  Path.Pident { stamp = -1; name = name; flags = 0 }

let create_fullname name =
  { txt = Longident.Lident (name); loc = loc }

(*****************************************************************************************************************************)

let expr_desc_to_expr ed =
  { exp_desc = ed; exp_loc = loc; exp_extra = []; exp_type = type_expr; exp_env = Env.empty; exp_attributes = [] }

let create_ident name =
  let path     = create_path name in
  let fullname = create_fullname name in
  let val_desc = {Types.val_type = type_expr; val_kind = Types.Val_reg; val_loc = loc; val_attributes = [] } in
  let exp_desc = Texp_ident (path, fullname, val_desc) in
  expr_desc_to_expr exp_desc

let create_fun param_name body =
  let param    = Ident.create param_name in
  let pat_desc = Tpat_var (param, { txt = param_name; loc = loc }) in
  let pat      = { pat_desc = pat_desc; pat_loc = loc; pat_extra = [];
                   pat_type = type_expr; pat_env = Env.empty; pat_attributes = [] } in
  let case     = { c_lhs = pat; c_guard = None; c_rhs = body; c_cont=None (* ?? *) } in
  let func     = Texp_function ("", [case], Partial) in
  expr_desc_to_expr func

let create_constructor name args =

  let arity = List.length args in

  let c_desc = { Types.cstr_name  = name;  cstr_res       = type_expr; cstr_existentials = [];
                 cstr_args        = [];    cstr_arity     = arity;     cstr_tag = Types.Cstr_constant 0;
                 cstr_consts      = 0;     cstr_nonconsts = 0;         cstr_normal = 0;
                 cstr_generalized = false; cstr_private   = Public;    cstr_loc = loc;
                 cstr_attributes  = []
                 (* ;    cstr_inlined   = None  *)
                }
  in

  let ident     = create_fullname name in
  let expr_desc = Texp_construct (ident, c_desc, args) in
  expr_desc_to_expr expr_desc

let create_apply e args =
  Texp_apply (e, List.map (fun a -> ("", Some a, Required)) args) |> expr_desc_to_expr

let create_let rec_flag var_name body rest =
  let var      = Ident.create var_name in
  let pat_desc = Tpat_var (var, { txt = var_name; loc = loc }) in
  let pat      = { pat_desc = pat_desc; pat_loc = loc; pat_extra = [];
                   pat_type = type_expr; pat_env = Env.empty; pat_attributes = [] } in
  let val_bind = { vb_pat = pat; vb_expr = body; vb_attributes = []; vb_loc = loc } in
  let exp_let  = Texp_let (rec_flag, [val_bind], rest) in
  expr_desc_to_expr exp_let

let create_inj e =
  let inj_ident = create_ident inj_name in
  create_apply inj_ident [e]

let create_unify e1 e2 =
  let uniq_ident = create_ident uniq_name in
  create_apply uniq_ident [e1; e2]

let create_des_constr e1 e2 =
  let des_constr_ident = create_ident des_constr_name in
  create_apply des_constr_ident [e1; e2]

let create_or e1 e2 =
  let or_ident = create_ident or_name in
  create_apply or_ident [e1; e2]

let create_and e1 e2 =
  let and_ident = create_ident and_name in
  create_apply and_ident [e1; e2]

let create_fresh var_name body =
  let fresh = create_ident fresh_name in
  let func  = create_fun var_name body in
  create_apply fresh [func]

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

type error = NotYetSupported of string
exception Error of error

let report_error fmt  = function
| NotYetSupported s -> Format.fprintf fmt "Not supported during relational conversion: %s\n%!" s

let get_translator start_index =

  (****)

  let curr_index  = ref start_index in

  (****)

  let logic t =
        match t.ctyp_desc with
        | Ttyp_var _ -> t
        | _          ->
          let path       = create_path logic_type_name in
          let fullname   = create_fullname logic_type_name in
  	  let type_desc  = Ttyp_constr (path, fullname, [t]) in
          { ctyp_desc = type_desc; ctyp_type = type_expr; ctyp_env = Env.empty; ctyp_loc = loc; ctyp_attributes = [] } in

  let rec type_to_logic_type x =
    let adder =
      let typ sub x = type_to_logic_type x in
      {Tast_mapper.default with typ} in

    x |> Tast_mapper.default.typ adder |> logic in

  let args_to_logic_args cd =
    let cd_args =
      List.map type_to_logic_type cd.cd_args
      (* match cd.cd_args with
      | Cstr_tuple l -> Cstr_tuple (List.map type_to_logic_type l) *)
    in
    {cd with cd_args}
  in

  (* let type_kind sub x =
    match x with
    | Ttype_variant l -> Ttype_variant (List.map args_to_logic_args l)
    | Ttype_record _  -> raise (Error (NotYetSupported "record types"))
    | Ttype_abstract  -> raise (Error (NotYetSupported "abstract types"))
    | Ttype_open      -> raise (Error (NotYetSupported "open types"))
  in *)
  (****)

  let create_fresh_var_name () =
    let name = Printf.sprintf "%s%d" fresh_var_prefix !curr_index in
    curr_index := !curr_index + 1;
    name in

  (****)

  let tarnslate_constant const_expr =
    let output_var_name   = create_fresh_var_name () in
    let output_var        = create_ident output_var_name in
    let inj_expr          = create_inj const_expr in
    let unify_const       = create_unify output_var inj_expr in
    create_fun output_var_name unify_const in

  (****)

  let translate_construct (sub : Tast_mapper.mapper) name def args =
    let output_var_name   = create_fresh_var_name () in
    let output_var        = create_ident output_var_name in

    let fresh_var_names   = List.map (fun _ -> create_fresh_var_name ()) args in
    let fresh_vars        = List.map create_ident fresh_var_names in

    let new_cnstr         = Texp_construct (name, def, fresh_vars) |> expr_desc_to_expr in
    let inj_cnstr         = create_inj new_cnstr in
    let unify_cnstr       = create_unify output_var inj_cnstr in

    let unifies_list      = List.map (sub.expr sub) args |> List.map2 (fun v e -> create_apply e [v]) fresh_vars in
    let cnstr_and_unifies = List.fold_left create_and unify_cnstr unifies_list in
    let translated_cnstr  = List.fold_right create_fresh fresh_var_names cnstr_and_unifies in
    create_fun output_var_name translated_cnstr in

  (****)

  let translate_match (sub : Tast_mapper.mapper) expr cases (typ : Types.type_expr) =

    let argument_names =
      let rec calculate (typ : Types.type_expr) =
        match typ.Types.desc with
        | Types.Tarrow (_, _, right_typ, _) -> let name = create_fresh_var_name () in
                                         name :: calculate right_typ
        | Types.Tlink typ                   -> calculate typ
        | _                           -> [create_fresh_var_name ()] in
      calculate typ in

    let arguments = List.map create_ident argument_names in

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
        ident.name in

      let real_arg_names    = List.map get_var_name real_arg_pats in
      let real_args         = List.map create_ident real_arg_names in

      let fresh_arg_names   = List.map (fun _ -> create_fresh_var_name ()) real_args in
      let fresh_args        = List.map create_ident fresh_arg_names in

      let translated_body   = sub.expr sub body in
      let body_with_args    = create_apply translated_body arguments in
      let body_with_lambda  = List.fold_right create_fun real_arg_names body_with_args in

      let lambda_arg_names  = List.map (fun _ -> create_fresh_var_name ()) real_args in
      let lambda_args       = List.map create_ident lambda_arg_names in

      let unifies_for_subst = List.map2 create_unify lambda_args fresh_args in
      let funs_for_subst    = List.map2 create_fun lambda_arg_names unifies_for_subst |> List.map (fun x -> [x]) in
      let body_with_subst   = List.fold_left create_apply body_with_lambda funs_for_subst in

      let cnstr_desc =
        match pattern.pat_desc with
        | Tpat_constant const          -> Texp_constant const
        | Tpat_construct (name, cd, _) -> Texp_construct (name, cd, fresh_args) in

      let cnstr             = cnstr_desc |> expr_desc_to_expr |> create_inj in
      let cnstr_unify       = create_unify unify_var cnstr in

      let cnstr_and_body    = create_and cnstr_unify body_with_subst in

      List.fold_right create_fresh fresh_arg_names cnstr_and_body in

    let translated_cases     = cases |> List.map translate_case |> fold_right0 create_or in
    let upper_exp_with_cases = create_and unify_expr translated_cases in
    let all_without_lambda   = create_fresh unify_var_name upper_exp_with_cases in

    List.fold_right create_fun argument_names all_without_lambda in

  (****)

  let translate_apply (sub : Tast_mapper.mapper) func args parent_type =

    let func = sub.expr sub func in

    let rec is_primary_type (t : Types.type_expr) =
      match t.desc with
      | Tarrow _ -> false
      | Tlink t' -> is_primary_type t'
      | _        -> true in

    let external_argument_names =
      let rec calculate (typ : Types.type_expr) =
        match typ.desc with
        | Tarrow (_, _, right_typ, _) -> let name = create_fresh_var_name () in
                                         name :: calculate right_typ
        | Tlink typ                   -> calculate typ
        | _                           -> [create_fresh_var_name ()] in
      calculate parent_type in

    let external_arguments = List.map create_ident external_argument_names in

    let args = List.map (fun (_, Some a,_) -> a) args in

    let is_primary_list = List.map (fun a -> is_primary_type a.exp_type) args in

    let args = List.map (sub.expr sub) args in

    let fresh_names_if_primary =
      List.map (fun a -> if a then Some (create_fresh_var_name ()) else None) is_primary_list in

    let update_arg name_opt arg =
      match name_opt with
      | Some name -> create_apply (create_ident uniq_name) [create_ident name]
      | _         -> arg in

    let new_args = List.map2 update_arg fresh_names_if_primary args in

    let apply = create_apply func (new_args @ external_arguments) in

    let calculated_args = List.combine fresh_names_if_primary args
                       |> List.filter (fun (n, _) -> n != None)
                       |> List.map (fun (Some n, a) -> create_apply a [create_ident n]) in

    let args_and_apply = List.fold_right create_and calculated_args apply in

    let create_fresh name_opt body =
      match name_opt with
      | Some name -> create_fresh name body
      | None      -> body in

    let with_fresh = List.fold_right create_fresh fresh_names_if_primary args_and_apply in

    List.fold_right create_fun external_argument_names with_fresh in

  (****)

  let translate_eq (sub : Tast_mapper.mapper) =

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

    let out_unify_true  = create_unify ident_out const_true in
    let out_unify_false = create_unify ident_out const_false in
    let l_unify_r       = create_unify ident_l ident_r in
    let l_des_constr_r  = create_des_constr ident_l ident_r in
    let first_and       = create_and l_unify_r out_unify_true in
    let second_and      = create_and l_des_constr_r out_unify_false in
    let full_or         = create_or first_and second_and in

    let apply_l = create_apply ident_arg_l [ident_l] in
    let apply_r = create_apply ident_arg_r [ident_r] in

    let party_and = create_and apply_r full_or in
    let full_and  = create_and apply_l party_and in

    let first_fresh  = create_fresh name_r full_and in
    let second_fresh = create_fresh name_l first_fresh in

    let fun1 = create_fun name_out second_fresh in
    let fun2 = create_fun name_arg_r fun1 in

    create_fun name_arg_l fun2 in

  (****)

  let expr sub x =
    match x.exp_desc with
    | Texp_constant _           -> tarnslate_constant x
    | Texp_construct (n, cd, l) -> translate_construct sub n cd l
    | Texp_match (e, cs, _, _, _)  -> translate_match sub e cs x.exp_type
    | Texp_apply (func, args)   -> translate_apply sub func args x.exp_type

    | Texp_ident (_, { txt = Longident.Lident name }, _) when name = eq_name -> translate_eq sub

    | _ -> Tast_mapper.default.expr sub x in

  let check_cd_is_ok (_cd: constructor_declaration list) = true in

  { Tast_mapper.default with expr
  (* ; type_kind = fun _ ->  *)
  ; type_declaration = fun _ decl ->
      match decl.typ_kind with
      | Ttype_variant cds when check_cd_is_ok cds ->
          { decl with typ_attributes = [(Location.mknoloc "put_distrib_here", Parsetree.PStr [])] }
      | Ttype_record _  -> raise (Error (NotYetSupported "record types"))
      | Ttype_abstract  -> raise (Error (NotYetSupported "abstract types"))
      | Ttype_open      -> raise (Error (NotYetSupported "open types"))
}

(*****************************************************************************************************************************)

let add_packages tast =
  let pos        = { pos_fname = ""; pos_lnum = -1; pos_bol = -1; pos_cnum = -1 } in
  let loc        = { loc_start = pos; loc_end = pos; loc_ghost = true } in
  let longidents = List.map (fun n -> Longident.Lident n) packages in
  let locs       = List.map (fun l -> { txt = l; loc = loc }) longidents in
  let pathes     = List.map (fun n -> Path.Pident { stamp = -1; name = n; flags = 0 }) packages in
  let open_desc  = List.map2 (fun i p -> { open_path = p; open_txt = i; open_override = Asttypes.Fresh;
                                           open_loc = loc; open_attributes = [] }) locs pathes in
  let item_descs = List.map (fun od -> Tstr_open od) open_desc in
  let str_items' = List.map (fun id -> { str_desc = id; str_loc = loc; str_env = Env.empty }) item_descs in
  let str_items  = List.append str_items' tast.str_items in
  { tast with str_items }


(*****************************************************************************************************************************)

let beta_reductor =

  let rec substitutor expr var subst =

    let name_from_pat pat =
      let Tpat_var (name, _) = pat.pat_desc in
      name.name in

    match expr.exp_desc with
    | Texp_ident (_, ident, _) ->
      let Longident.Lident name = ident.txt in
      if name = var then subst else expr

    | Texp_function (label, [case], typ) ->
      if name_from_pat case.c_lhs = var then expr else
        let c_rhs    = substitutor case.c_rhs var subst in
        let new_case = { case with c_rhs } in
        let exp_desc = Texp_function (label, [new_case], typ) in
        { expr with exp_desc }

    | Texp_apply (func, args) ->
      let new_func = substitutor func var subst in
      let new_args = List.map (fun (l, Some arg, flg) -> (l, Some (substitutor arg var subst), flg)) args in
      let exp_desc = Texp_apply (new_func, new_args) in
      { expr with exp_desc }

    | Texp_let (rec_flag, val_binds, rest) ->

      let is_recursive = rec_flag == Recursive in

      let var_in_binds =
        let all_names = List.map (fun vb -> name_from_pat vb.vb_pat) val_binds in
        List.exists ((=)var) all_names in

      let subst_in_vb val_bind =
        if (is_recursive && var_in_binds) || (not is_recursive && var = (name_from_pat val_bind.vb_pat)) then val_bind else
          let vb_expr = substitutor val_bind.vb_expr var subst in
          { val_bind with vb_expr } in

      let new_vbs  = List.map subst_in_vb val_binds in
      let new_rest = if var_in_binds then rest else substitutor rest var subst in
      let exp_desc = Texp_let (rec_flag, new_vbs, new_rest) in
      { expr with exp_desc }
    | _ -> expr in

  let rec beta_reduction expr args =
    match expr.exp_desc with
    | Texp_apply (func, args') ->
      let old_args = List.map (fun (_, Some a, _) -> a) args' in
      let new_args = List.map (fun a -> beta_reduction a []) old_args in
      List.append new_args args |> beta_reduction func

    | Texp_function (_, [case], _) ->
      let Tpat_var (var, _) = case.c_lhs.pat_desc in
      begin
        match args with
        | arg::args' ->
          let new_body = substitutor case.c_rhs var.name arg in
          beta_reduction new_body args'

        | _          ->
          let new_body = beta_reduction case.c_rhs [] in
          create_fun var.name new_body
      end

    | Texp_let (rec_flag, val_binds, rest) ->
      let new_vbs  = List.map (fun v -> let vb_expr = beta_reduction v.vb_expr [] in { v with vb_expr } ) val_binds in
      let new_rest = beta_reduction rest args in
      let exp_desc = Texp_let (rec_flag, new_vbs, new_rest) in
      { expr with exp_desc }

    | _ ->
      match args with
      | [] -> expr
      | _  -> create_apply expr args in


  let expr _ x = beta_reduction x [] in


    { Tast_mapper.default with expr }

(*****************************************************************************************************************************)

end

let print_if ppf flag printer arg =
  if !flag then Format.fprintf ppf "%a@." printer arg;
  arg

(* registering actual translator *)
let () =
  Typemod.ImplementationHooks.add_hook "ml_to_mk"
    (fun
      (hook_info : Misc.hook_info)
      ((tast, coercion) : Typedtree.structure * Typedtree.module_coercion) ->
      let open Lozov  in
      let current_index = get_max_index tast + 1 in
      let translator    = get_translator current_index in
      let new_tast      = translator.structure translator tast |> add_packages in
      let need_reduce   = true in
      let reduced_tast  = if need_reduce
                          then beta_reductor.structure beta_reductor new_tast
                          else new_tast in

      let new_ast       =
        Untypeast.untype_structure reduced_tast |>
        print_if Format.std_formatter Clflags.dump_parsetree Printast.implementation |>
        print_if Format.std_formatter Clflags.dump_source Pprintast.structure
      in

      try
        let (retyped_ast,_,_env) =
          Printexc.print
          (Typemod.type_structure (Compmisc.initial_env()) new_ast)
          Location.none
        in
        (retyped_ast, coercion)
      with
        | Error e as exc ->
          report_error Format.std_formatter e;
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
    );
