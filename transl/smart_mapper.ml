open Printf
open Typedtree
open Asttypes
open Ast_helper

module Untypeast = struct
  include Untypeast

  let untype_pattern {pat_desc; pat_loc=loc} =
    match pat_desc with
    | Tpat_any -> Pat.any ~loc ()
    | Tpat_var (_,str) -> Pat.var ~loc str
    | _ -> failwith "Not implemented"
end
module Exp = struct
  include Exp
  let of_longident ?(loc=Location.none) lident =
    { Parsetree.pexp_attributes = []
    ; pexp_loc = loc
    ; pexp_desc = Pexp_ident (Location.mkloc lident loc)
    }
end

class virtual ['self] smart_mapper = object (self: 'self)
  method virtual type_declaration: 'inh_tdecl -> type_declaration -> Parsetree.type_declaration
  method virtual expression: 'inh_expr -> expression -> Parsetree.expression
(*  method virtual match_: -> 'inh_expr -> Parsetree.expression*)
end



class virtual ['self] transformation = object(self: 'self)
  inherit ['self ] smart_mapper

  method type_declaration _ = Untypeast.untype_type_declaration

  method expression inh e = match e.exp_desc with
    | Texp_constant _           -> self#translate_constant e
    | Texp_construct (n, _cd, es) -> self#translate_construct n.txt n.loc es
    (*#if OCAML_VERSION > (4, 02, 2)
    | Texp_match (e, cs, _, _)  -> translate_match sub e cs x.exp_type
    #else*)
    | Texp_match (e, cs, _, _, _)  -> self#translate_match e cs e.exp_type
(*    #endif*)
    | Texp_apply (func, args)   -> self#translate_apply func args e.exp_type
    | Texp_function (label, [{c_lhs=p; c_guard=None; c_rhs=e}], _) ->
        Exp.fun_ label None (Untypeast.untype_pattern p) (self#expression inh e)
    | Texp_function ("", cases, _) ->
        let untype_case {c_lhs; c_guard; c_rhs} =
          { Parsetree.pc_lhs = Untypeast.untype_pattern c_lhs
          ; pc_rhs = self#expression inh c_rhs
          ; pc_guard = None (* TODO: finish implementation *)
          }
        in
        Exp.function_ (List.map untype_case cases)
    | Texp_function _ -> assert false
    | Texp_ident (_, { txt = Longident.Lident name }, _) when name = "===" -> self#translate_eq e
    | _ -> Untypeast.untype_expression e

  method structure {str_items; str_type; str_final_env} =
    List.concat @@ List.map (self#structure_item str_type str_final_env) str_items
  method structure_item _typ _env item =
    match item.str_desc with
    | Tstr_type tdecls -> Untypeast.untype_structure {str_items=[item]; str_type=_typ; str_final_env = _env }
    | Tstr_value (flg, vbs) -> self#value_bindings flg vbs

  method virtual value_bindings : rec_flag -> Typedtree.value_binding list -> Parsetree.structure_item list
  method virtual translate_constant : expression -> Parsetree.expression
  method virtual translate_construct: Longident.t -> Location.t -> expression list -> Parsetree.expression
  method virtual translate_match : expression -> _ -> _ -> Parsetree.expression
  method virtual translate_eq : expression -> Parsetree.expression
  method virtual translate_apply :
        Typedtree.expression ->
        (Asttypes.label * Typedtree.expression option * Typedtree.optional) list ->
        Types.type_expr ->
        Parsetree.expression
end

let map_deepest_lident ~f lident =
  let open Longident in
  let rec helper = function
    | Lident s -> Lident (f s)
    | Ldot (l, s) -> Ldot (l, f s)
    | Lapply (l, r) -> Lapply (l, helper r)
  in
  helper lident

class ['self] transformation_impl = object(self: 'self)
  inherit ['self] transformation

  val fresh_var_counter = ref 0
  val fresh_var_prefix = "q"
  method create_fresh_var_name =
    let name = sprintf "%s%d" fresh_var_prefix !fresh_var_counter in
    incr fresh_var_counter;
    name

  method value_bindings flg xs =
    (*List.map (fun {vb_expr;vb_loc} -> [%stri let x = 1]) xs*)
    List.map (fun {vb_expr;vb_loc; vb_pat} ->
      Str.value flg [Vb.mk (Untypeast.untype_pattern vb_pat) @@ self#expression () vb_expr]
    ) xs
  method translate_constant _ = assert false
  method translate_construct lident loc args =
    Exp.of_longident ~loc (map_deepest_lident Util.mangle_construct_name lident)


  method translate_match _ _ typ_scrutinee =
    let argument_names =
      let rec calculate (typ : Types.type_expr) =
        match typ.Types.desc with
        | Types.Tarrow (_, _, right_typ, _) ->
              let name = self#create_fresh_var_name in
              name :: calculate right_typ
        | Types.Tlink typ             -> calculate typ
        | _                           -> [self#create_fresh_var_name]
      in
      calculate typ_scrutinee
    in
    assert false

  method translate_eq _ = assert false
  method translate_apply _ _ _ = assert false
end

let process tast =
  (new transformation_impl)#structure tast

