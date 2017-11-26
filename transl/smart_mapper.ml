open Typedtree


class virtual ['self] smart_mapper = object (self: 'self)
  method virtual type_declaration: 'inh_tdecl -> type_declaration -> Parsetree.type_declaration
  method virtual expression: 'inh_expr -> expression -> Parsetree.expression
(*  method virtual match_: -> 'inh_expr -> Parsetree.expression*)
end


(*
let mmm = object(self: 'self)
  inherit ['self ] smart_mapper

  method type_declaration _ = Untypeast.untype_type_declaration

  method expression inh e = match e.exp_desc with
    | Texp_constant _           -> self#translate_constant e
    | Texp_construct (n, cd, l) -> self#translate_construct sub n cd l
    (*#if OCAML_VERSION > (4, 02, 2)
    | Texp_match (e, cs, _, _)  -> translate_match sub e cs x.exp_type
    #else*)
    | Texp_match (e, cs, _, _, _)  -> self#translate_match e cs e.exp_type
(*    #endif*)
    | Texp_apply (func, args)   -> self#translate_apply sub func args e.exp_type

    | Texp_ident (_, { txt = Longident.Lident name }, _) when name = "===" -> self#translate_eq e
    | _ -> Untypeast.untype_expression e

  method virtual translate_constant : expression -> Parsetree.expression
  method virtual translate_match : expression -> _ -> _ -> Parsetree.expression
  method virtual translate_constant : expression -> Parsetree.expression
  method virtual translate_eq : expression -> Parsetree.expression
end
*)
