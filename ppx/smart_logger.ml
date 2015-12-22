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

let need_insert_fname ~name e =
  match e.pexp_desc with
  | Pexp_ident i when i.txt = Lident name -> true
  | _ -> false

let is_call_fresh = need_insert_fname ~name:"call_fresh"
let is_conj = need_insert_fname ~name:"conj"
let is_disj = need_insert_fname ~name:"disj"

let rec walkthrough ~fname (expr: expression) =

  let add_fname () =
    [%expr [%e Ast_helper.Exp.constant (Const_string (fname,None))] <=>
           [%e expr]
    ]
  in
  match expr.pexp_desc with
  | Pexp_fun (_label, _opt, pat, e2) -> begin
      match is_state_pattern pat with
      | None ->
        { expr with pexp_desc =
                   Pexp_fun (_label, _opt, pat, walkthrough ~fname e2) }
      | Some argname ->
        (* printf "found good function with statearg '%s'\n%!" argname; *)
        let new_body =
          [%expr
             let () = Printf.printf "entering '%s'\n%!" [%e Ast_helper.Exp.constant (Const_string (fname,None))] in
             let ans = [%e e2] in
             let () = Printf.printf "leaving '%s'\n%!"  [%e Ast_helper.Exp.constant (Const_string (fname,None))] in
             ans
          ]
        in
        { expr with pexp_desc= Pexp_fun (_label, _opt, pat, new_body) }
    end
  | Pexp_apply (e,_) when is_call_fresh e -> add_fname ()
  | Pexp_apply (e,_) when is_disj e -> add_fname ()
  | Pexp_apply (e,_) when is_conj e -> add_fname ()

  | _ -> expr


let map_value_binding (vb : value_binding) =
  match vb.pvb_pat.ppat_desc with
  | Ppat_var name ->
    let fname = name.txt in
    { vb with pvb_expr = walkthrough ~fname vb.pvb_expr }
  |  _ -> vb

let smart_logger argv =
  { default_mapper with
    structure_item = fun mapper sitem ->
      match sitem.pstr_desc with
      | Pstr_value (_rec, vbs) ->
        { sitem with pstr_desc = Pstr_value (_rec, List.map vbs ~f:map_value_binding) }
      | x -> default_mapper.structure_item mapper sitem
  }


let option_map ~f = function Some x -> Some (f x) | None -> None
let option_bind ~f = function Some x -> f x | None -> None

exception Not_an_ident
let reconstruct_args e =
  let are_all_idents (xs: (_*expression) list) =
    try Some (List.map (fun (_,e) ->
                        match e.pexp_desc with
                        | Pexp_ident {txt=(Longident.Lident i);_} -> i
                        | _ -> raise Not_an_ident) xs)
    with Not_an_ident -> None
  in
  match e.pexp_desc with
  | Pexp_apply ({pexp_desc=Pexp_ident {txt=Longident.Lident arg1; _}}, ys) -> begin
      option_map (are_all_idents ys) ~f:(fun xs -> arg1::xs )
    end
  | _ -> None

let rec pamk_e mapper e : expression =
  match e.pexp_desc with
  | Pexp_apply (e1,[(_,args); (_,body)]) -> begin
      let new_body : expression = pamk_e mapper body in
      match reconstruct_args args with
      | Some xs ->
         let (_: string list) = xs in
         let new_body = List.fold_right
                          ~f:(fun ident acc ->
                              [%expr call_fresh (fun [%p pvar ident ] -> [%e acc]) ]
                             )
                          ~init:new_body xs
         in
         new_body
         (* {e with pexp_desc=Pexp_apply (e1,["",new_body]) } *)
      | None -> {e with pexp_desc=Pexp_apply (e1,["",new_body]) }
    end
  | Pexp_apply (e, xs) ->
     {e with pexp_desc=Pexp_apply (mapper.expr mapper e,
                                   List.map ~f:(fun (lbl,e) -> (lbl, mapper.expr mapper e)) xs ) }
  | Pexp_fun (l,opt,pat,e) ->
     { e with pexp_desc=Pexp_fun(l,opt,pat, mapper.expr mapper e) }

  (* TODO: support all cases *)
  | _ -> e


let pa_minikanren argv =
  { default_mapper with expr = fun mapper e -> pamk_e mapper e
  }
