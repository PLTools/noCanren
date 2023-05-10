open Untypeast
open Typedtree
open Longident

let skip_bindings =
  { default_mapper with
    expr =
      (fun self e ->
        match e.exp_desc with
        | Texp_let (_, [ { vb_attributes } ], body)
          when Util.has_named_attribute "only_lozovml" vb_attributes ->
          self.expr self body
        | _ -> default_mapper.expr self e)
  }
;;

let untype_types_sign =
  let open Ast_helper in
  let rec untype_item = function
    | Types.Sig_value (id, vd, _) ->
      Sig.value (Val.mk (Location.mknoloc (Ident.name id)) (untype_type vd.val_type))
    | Types.Sig_type (id, tdecl, is_rec, _) ->
      Sig.type_
        Nonrecursive
        (List.map (fun f -> f (Location.mknoloc (Ident.name id))) (untype_tdecl tdecl))
    | _ -> failwith "Unexpected signature"
  and untype_tdecl decl =
    let params =
      List.map
        (fun t -> untype_type t, (Asttypes.NoVariance, Asttypes.NoInjectivity))
        decl.type_params
    in
    let manifest =
      match decl.type_manifest with
      | None -> assert false
      | Some t -> untype_type t
    in
    [ Type.mk ~manifest ~params ]
  and lident_of_path = function
    | Path.Pident id -> Lident (Ident.name id)
    | Path.Pdot (p, s) -> Ldot (lident_of_path p, s)
    | _ -> assert false
  and untype_type t =
    let loc = Location.none in
    match Types.get_desc t with
    | Tvar None -> [%type: _]
    | Tvar (Some name) -> Typ.var name
    | Tconstr (path, args, _) ->
      let id = lident_of_path path in
      Typ.constr (Location.mknoloc id) (List.map untype_type args)
    | Tarrow (Asttypes.Nolabel, l, r, _) ->
      [%type: [%t untype_type l] -> [%t untype_type r]]
    | _ -> Location.raise_errorf "%s %d" __FILE__ __LINE__
  in
  List.map untype_item
;;

let untype_module_type mt =
  match mt.mty_desc with
  | Tmty_ident (_, { txt = Lident ident; loc }) ->
    Ast_helper.Mty.ident (Location.mkloc (Lident ident) loc)
  | Tmty_signature s -> Ast_helper.Mty.signature @@ untype_signature s
  | _ -> assert false
;;

let untype_functor_param = function
  | Typedtree.Unit -> Parsetree.Unit
  | Named (_, name, mt) -> Parsetree.Named (name, untype_module_type mt)
;;
