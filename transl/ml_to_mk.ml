
(* Print all fully qualified names in expressions *)

open Asttypes
open Longident
open Typedtree


open Tast_mapper

let tast_mapper =
  { default with
    expr = (fun mapper expr ->
      match expr with
      | { exp_desc = Texp_ident (path, _lid, _valdesc) }
        ->
        Printf.printf "Ident %S\n%!" (Path.name path);
          expr
      | other -> default.expr mapper other); }

let () =
  Typemod.ImplementationHooks.add_hook "pptx_example"
    (fun
      (hook_info : Misc.hook_info)
      ((ast, coercion) : Typedtree.structure * Typedtree.module_coercion) ->
        let ast = tast_mapper.structure tast_mapper ast in
        (ast, coercion)
    );
  Typemod.InterfaceHooks.add_hook "pptx_example"
    (fun (hook_info : Misc.hook_info)
      (ast : Typedtree.signature) ->

        ast);
  ()
