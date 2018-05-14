(* Print all fully qualified names in expressions *)
open Printf
open Asttypes
open Longident
open Typedtree
open Tast_mapper
open Smart_mapper

let () = Printexc.record_backtrace true

module Lozov = struct
open Typedtree
open Ident
open Asttypes
open Lexing
open Location

(*****************************************************************************************************************************)

let dummy_name          = "dummy"

let eq_name             = "="
let neq_name            = "<>"
let true_name           = "true"
let false_name          = "false"
let bool_and_name       = "&&"
let bool_or_name        = "||"
let bool_not_name       = "not"

let logic_type_name     = "logic"
let inj_name            = "!!"
let uniq_name           = "==="
let des_constr_name     = "=/="
let or_name             = "|||"
let and_name            = "&&&"
let fresh_name          = "call_fresh"
let fresh_var_prefix    = "q"

let tabling_module_name = "Tabling"
let tabling_one_name    = "one"
let tabling_succ_name   = "succ"
let tabling_rec_name    = "tabledrec"

let tabling_attr_name   = "tabled"

(*****************************************************************************************************************************)

let packages = ["MiniKanren"; "MiniKanrenStd"]

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

let nolabel =
#if OCAML_VERSION > (4, 02, 2)
      Asttypes.Nolabel
#else
      ""
#endif

let expr_desc_to_expr ed =
  { exp_desc = ed; exp_loc = loc; exp_extra = []; exp_type = type_expr; exp_env = Env.empty; exp_attributes = [] }

let create_ident name =
  let path     = create_path dummy_name in
  let fullname = create_fullname name in
  let val_desc = {Types.val_type = type_expr; val_kind = Types.Val_reg; val_loc = loc; val_attributes = [] } in
  let exp_desc = Texp_ident (path, fullname, val_desc) in
  expr_desc_to_expr exp_desc

let create_ident_with_dot nl nr =
  let path     = create_path dummy_name in
  let fullname = { txt = Longident.parse (nl ^ "." ^ nr); loc = loc } in
  let val_desc = {Types.val_type = type_expr; val_kind = Types.Val_reg; val_loc = loc; val_attributes = [] } in
  let exp_desc = Texp_ident (path, fullname, val_desc) in
  expr_desc_to_expr exp_desc

let create_fun param_name body =
  let param    = Ident.create param_name in
  let pat_desc = Tpat_var (param, { txt = param_name; loc = loc }) in
  let pat      = { pat_desc = pat_desc; pat_loc = loc; pat_extra = [];
                   pat_type = type_expr; pat_env = Env.empty; pat_attributes = [] } in
  let case     = { c_lhs = pat; c_guard = None; c_rhs = body
#if OCAML_VERSION = (4, 02, 2)
      ; c_cont=None (* ?? *)
#endif
  } in
  let func     =
    let cases = [case] in
    let partial = Partial in
#if OCAML_VERSION > (4, 02, 2)
    let arg_label = Nolabel in
    Texp_function {arg_label; param; cases; partial}
#else
    let arg_label = "" in
    Texp_function ("", cases, partial)
#endif
  in
  expr_desc_to_expr func

let create_constructor name args =

  let arity = List.length args in

  let c_desc = { Types.cstr_name  = name;  cstr_res       = type_expr; cstr_existentials = [];
                 cstr_args        = [];    cstr_arity     = arity;     cstr_tag = Types.Cstr_constant 0;
                 cstr_consts      = 0;     cstr_nonconsts = 0;         cstr_normal = 0;
                 cstr_generalized = false; cstr_private   = Public;    cstr_loc = loc;
                 cstr_attributes  = []
#if OCAML_VERSION > (4, 02, 2)
                  ;    cstr_inlined   = None
#endif
}
  in

  let ident     = create_fullname name in
  let expr_desc = Texp_construct (ident, c_desc, args) in
  expr_desc_to_expr expr_desc

let create_apply e args =
#if OCAML_VERSION > (4, 02, 2)
  let make a = (Nolabel, Some a) in
#else
  let make a = ("", Some a , Required) in
#endif
  Texp_apply (e, List.map make args) |> expr_desc_to_expr

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

let create_conde xs =
  let cnde = create_ident "conde" in
  create_apply cnde [List.fold_right (fun x acc -> (create_constructor "::" [x;acc])) xs (create_constructor "[]" [])]

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

let dummy_val_desc =
  let open Types in
  let val_kind = Val_reg in
  let val_type = { desc = Tvar (Some "a"); level = 0; id = 0} in
  let val_loc = Location.none in
  {val_kind; val_type; val_loc; val_attributes = []}

(*****************************************************************************************************************************)

type error = NotYetSupported of string
exception Error of error

let report_error fmt  = function
| NotYetSupported s -> Format.fprintf fmt "Not supported during relational conversion: %s\n%!" s

let get_translator start_index need_sort_goals need_unlazy =

  (****)

  let curr_index     = ref start_index in
  let curr_rec_names = ref [] in

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
      let wrap f cd =
#if OCAML_VERSION > (4, 02, 2)
        match cd.cd_args with
        | Cstr_tuple l -> Cstr_tuple (List.map type_to_logic_type l)
#else
        f cd.cd_args
#endif
      in
      wrap (List.map type_to_logic_type) cd
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

  let create_fresh_var_name () =
    let name = Printf.sprintf "%s%d" fresh_var_prefix !curr_index in
    curr_index := !curr_index + 1;
    name in

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
    | Texp_constant _  -> create_inj expr, []
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
    let translated_cnstr  = List.fold_right create_fresh var_names cnstr_and_unifies in
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

      List.fold_right create_fresh fresh_arg_names cnstr_and_body
    in

    let translated_cases     = cases |> List.map translate_case |> create_conde in

    let upper_exp_with_cases = if need_sort_goals && expr_has_rec_vars expr then create_and translated_cases unify_expr
                                                                            else create_and unify_expr translated_cases in

    let all_without_lambda   = create_fresh unify_var_name upper_exp_with_cases in

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
    let with_fresh              = List.fold_right create_fresh fresh_vars full_conj in
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
   let body_with_fresh = create_fresh cond_var_name cond_with_cases in
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

    let first_fresh  = create_fresh name_r full_and in
    let second_fresh = create_fresh name_l first_fresh in

    let fun1 = create_fun name_out second_fresh in
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

    let with_fresh = create_fresh name_l full_and in

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

    let with_fresh  = create_fresh fr_arg full_and in
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
        let freshing_and_recfunc  = List.fold_right create_fresh argument_names2 conjs_and_recfunc in
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
        let freshing_and_tabled   = List.fold_right create_fresh argument_names6 conjs_and_tabled in
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
      let name = last ident.txt in
      if name = var then subst else expr
#if OCAML_VERSION > (4, 02, 2)
    | Texp_function {arg_label; param; cases=[case]; partial} ->
        if name_from_pat case.c_lhs = var then expr else
        let c_rhs    = substitutor case.c_rhs var subst in
        let new_case = { case with c_rhs } in
        let exp_desc = Texp_function {arg_label; param; cases=[new_case]; partial} in
        { expr with exp_desc }
#else
    | Texp_function (label, [case], typ) ->
      if name_from_pat case.c_lhs = var then expr else
        let c_rhs    = substitutor case.c_rhs var subst in
        let new_case = { case with c_rhs } in
        let exp_desc = Texp_function (label, [new_case], typ) in
        { expr with exp_desc }
#endif
    | Texp_apply (func, args) ->
      let new_func = substitutor func var subst in
      let new_args =
#if OCAML_VERSION > (4, 02, 2)
      let make (l, Some arg)      = (l, Some (substitutor arg var subst)     ) in
#else
      let make (l, Some arg, flg) = (l, Some (substitutor arg var subst), flg) in
#endif
        List.map make args
      in
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
    | Texp_construct (name, desc, exprs) ->
      let new_exprs = List.map (fun x -> substitutor x var subst) exprs in
      let exp_desc = Texp_construct (name, desc, new_exprs) in
      {  expr with exp_desc }
    | _ -> expr in

  let rec beta_reduction expr args =
    match expr.exp_desc with
    | Texp_apply (func, args') ->
#if OCAML_VERSION > (4, 02, 2)
      let old_args = List.map (fun (_, Some a   ) -> a) args' in
#else
      let old_args = List.map (fun (_, Some a, _) -> a) args' in
#endif
      let new_args = List.map (fun a -> beta_reduction a []) old_args in
      List.append new_args args |> beta_reduction func

#if OCAML_VERSION > (4, 02, 2)
      | Texp_function {arg_label; param; cases=[case]; partial} ->
#else
      | Texp_function (_, [case], _) ->
#endif
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

    | Texp_construct (name, desc, exprs) ->
      let new_exprs = List.map (fun x -> beta_reduction x []) exprs in
      let exp_desc = Texp_construct (name, desc, new_exprs) in
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


let only_generate ~oldstyle hook_info tast =
  if oldstyle
  then try
    (*printf "Translating file %s\n%!" hook_info.Misc.sourcefile;
    Format.printf ">>>>\n%!";
    Pprintast.structure Format.std_formatter @@ Untypeast.untype_structure tast;
    Format.printf "\n<<<<\n%!";*)
    let open Lozov  in
    let need_sort_goals = true in
    let need_unlazy     = true in
    let current_index = get_max_index tast + 1 in
    let translator    = get_translator current_index need_sort_goals need_unlazy in
    let new_tast      = translator.structure translator tast |> add_packages in
    let need_reduce   = true in
    let reduced_tast  = if need_reduce
                        then beta_reductor.structure beta_reductor new_tast
                        else new_tast in

    Untypeast.untype_structure reduced_tast |>
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
