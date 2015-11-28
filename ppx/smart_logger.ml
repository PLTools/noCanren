open Printf
open Ast_mapper
open Ast_helper
open Asttypes
open Parsetree
open Longident
open StdLabels
open Ast_convenience

let is_state_pattern pat =
  match pat.ppat_desc with
  | Ppat_var v when v.txt = "st" || v.txt = "state" -> Some v.txt
  | _ -> None

let rec walkthrough ~fname (edesc: expression_desc) =
  match edesc with
  | Pexp_fun (_label, _opt, pat, e2) -> begin
      match is_state_pattern pat with
      | None ->
        Pexp_fun (_label, _opt, pat, {e2 with pexp_desc = walkthrough ~fname e2.pexp_desc})
      | Some argname ->
        printf "found good function with statearg '%s'\n%!" argname;
        let new_body =
          [%expr
             let () = Printf.printf "entering '%s'\n%!" [%e Ast_helper.Exp.constant (Const_string (fname,None))] in
             let ans = [%e e2] in
             let () = Printf.printf "leaving '%s'\n%!"  [%e Ast_helper.Exp.constant (Const_string (fname,None))] in
             ans
          ]
        in
        Pexp_fun (_label, _opt, pat, new_body)
    end
  | _ -> edesc


let map_value_binding (vb : value_binding) =
  match vb.pvb_pat.ppat_desc with
  | Ppat_var name ->
    let fname = name.txt in
    { vb with pvb_expr = { vb.pvb_expr with pexp_desc = walkthrough ~fname vb.pvb_expr.pexp_desc } }
  |  _ -> vb

let module_duplicate_mapper argv =
  { default_mapper with
    structure_item = fun mapper sitem ->
      match sitem.pstr_desc with
      | Pstr_value (_rec, vbs) ->
        { sitem with pstr_desc = Pstr_value (_rec, List.map vbs ~f:map_value_binding) }
      | x -> default_mapper.structure_item mapper sitem
  }


let () = register "module_duplicate" module_duplicate_mapper
