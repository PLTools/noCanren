open Printf
open Ast_mapper
open Ast_helper
open Parsetree

module TypeNameMap = Map.Make(String)

module FoldInfo = struct
  type item = {param_name:string; rtyp: core_type; ltyp: core_type}
  exception ItemFound of item
  type t = item list

  let param_for_rtyp typ ts =
    let typ_repr =
      Pprintast.core_type Format.str_formatter typ;
      Format.flush_str_formatter ()
    in
    try List.iter (fun i ->
                    let new_repr =
                      Pprintast.core_type Format.str_formatter i.rtyp;
                      Format.flush_str_formatter ()
                    in
                    if new_repr = typ_repr then raise (ItemFound i)
                  ) ts;
        None
    with ItemFound i -> Some i

    let map ~f (xs: t) = List.map f xs
    let empty = []
    let is_empty : t -> bool = (=) []
    let extend param_name rtyp ltyp ts =
(*      printf "extending by `%s`\n%!" param_name;*)
      {param_name; rtyp; ltyp} :: ts
end

let extract_names = List.map (fun typ ->
    match typ.ptyp_desc with
    | Ptyp_var s -> s
    | _ ->
      Buffer.clear Format.stdbuf;
      Pprintast.core_type Format.str_formatter typ;
      failwith (sprintf "Don't know what to do with %s" (Format.flush_str_formatter ()))
  )

let str_type_ = Ast_helper.Str.type_
(* let str_type_ flg =
 * #if OCAML_VERSION > (4, 02, 2)
 *       fun xs -> Ast_helper.Str.type_ xs
 * #else
 *       fun ~loc xs -> Ast_helper.Str.type_ ~loc xs
 * #endif *)

let nolabel =
#if OCAML_VERSION > (4, 02, 2)
      Asttypes.Nolabel
#else
      ""
#endif

let get_param_names pcd_args =
#if OCAML_VERSION > (4, 02, 2)
  let Pcstr_tuple pcd_args  = pcd_args in
#endif
  extract_names pcd_args

#if OCAML_VERSION <= (4, 02, 2)
type rec_flg_t = Nonrecursive | Recursive
#endif



let lower_lid lid = Location.{lid with txt = Util.mangle_construct_name lid.Location.txt }


let prepare_distribs ~loc tdecl fmap_decl =
  let open Location in
  let open Longident in
  let Ptype_variant constructors = tdecl.ptype_kind in

  let gen_module_str = mknoloc @@ "For_" ^ tdecl.ptype_name.txt in
  let distrib_lid = mknoloc Longident.(Ldot (Lident gen_module_str.txt, "distrib")) in
  [ Str.module_ @@ Mb.mk gen_module_str @@
      Mod.(apply (ident (mknoloc @@ Lident (let n = List.length tdecl.ptype_params in if n = 1 then "Fmap" else sprintf "Fmap%d" n))) @@ structure
        [ fmap_decl
        ; str_type_ Recursive ~loc [ Type.mk ~params:tdecl.ptype_params
            ~kind:Ptype_abstract
            ~manifest:(Typ.constr (mknoloc @@ Lident tdecl.ptype_name.txt) @@ List.map fst tdecl.ptype_params)
            (mknoloc "t") ]
        ])
  ; Str.value Recursive @@ List.map (fun {pcd_name; pcd_args} ->
        let names =
          let Pcstr_tuple xs = pcd_args in
          List.mapi (fun n _ -> Printf.sprintf "x__%d" n) xs
        in

      let open Exp in
      let body =
        let constr_itself = construct @@ mknoloc (Lident pcd_name.txt) in
        match names with
        | [] -> constr_itself None
        | [one] -> constr_itself (Some (ident @@ mknoloc (Lident one)))
        | xs -> (* construct tuple here *)
            constr_itself (Some (tuple @@ List.map (fun name -> ident @@ mknoloc (Lident name)) xs))
      in
      let body = [%expr inj [%e Exp.apply (Exp.ident distrib_lid) [nolabel, body] ] ] in
      Vb.mk (Pat.var @@ lower_lid pcd_name)
        (match names with
        | [] -> Exp.fun_ nolabel None (Pat.construct (mknoloc (Lident "()")) None) body
        | names -> List.fold_right (fun name acc -> Exp.fun_ nolabel None (Pat.var @@ mknoloc name) acc) names body)
    ) constructors
  ]

let prepare_fmap ~loc tdecl =
  let open Location in
  let open Ast_helper in

  let param_names = extract_names (List.map fst tdecl.ptype_params) in
  let Ptype_variant constructors = tdecl.ptype_kind in
  let cases = constructors |> List.map (fun {pcd_name; pcd_args} ->
    let argnames = get_param_names pcd_args in
    let cname = pcd_name.txt in
    let clid = mknoloc @@ Longident.Lident cname in
    let rec make_f_expr name =
      (*if name = "self" then apply_self ()
      else *)Exp.ident @@ mknoloc @@ Longident.Lident ("f"^name)
    (*and apply_self () =
      match List.filter ((<>)"self") argnames with
      | [] -> [%expr fmap]
      | xs -> Exp.(apply [%expr fmap] @@
                List.map (fun s -> ("", ident @@ mknoloc @@ Longident.Lident ("f"^s))) xs)*)
    in
    let pc_lhs, pc_rhs =
      let wrap_one_arg typname new_name =
        Exp.(apply (make_f_expr typname) [nolabel, ident @@ mknoloc @@ Longident.Lident new_name] )
      in
      let get_pat_name i name = sprintf "%s_%d" name i in
      match argnames with
      | [] -> (None, None)
      | [s] -> Some (Ast_helper.Pat.var (mknoloc s)), (Some (wrap_one_arg s s))
      | ___ ->
          Some Ast_helper.Pat.(tuple @@ List.mapi (fun n name -> var (mknoloc @@ get_pat_name n name)) argnames),
          Some (Exp.tuple @@ List.mapi (fun n name -> wrap_one_arg name @@ get_pat_name n name) argnames)
    in
    let pc_lhs = Ast_helper.Pat.construct clid pc_lhs in
    let pc_rhs = Exp.construct clid pc_rhs in
    let pc_guard = None in
    { pc_rhs; pc_lhs; pc_guard }
  )
  in

  [%stri let rec fmap = [%e
    List.fold_right (function
      | name -> Exp.fun_ nolabel None Pat.(var @@ mknoloc ("f"^name))
      ) param_names (Exp.function_ cases) ] ]

let revisit_adt ~loc tdecl ctors =
  let tdecl = {tdecl with ptype_attributes =
    List.filter (fun (name,_) -> name.Location.txt <> "put_distrib_here") tdecl.ptype_attributes }
  in
  let der_typ_name = tdecl.ptype_name.Asttypes.txt in
  (* Let's forget about mutal recursion for now *)
  (* For every constructor argument we need to put ground types to parameters *)
  let mapa, full_t =
    List.fold_right
      (fun cd (n, acc_map,cs) ->
          let n,map2,new_args = List.fold_right
            (fun typ (n, map,args) ->
                  match typ with
                  | [%type: _] -> assert false
                  | {ptyp_desc = Ptyp_var s; _} -> (n, map, typ::args)
                  | arg ->
                      match FoldInfo.param_for_rtyp arg map with
                      | Some {param_name; } -> (n, map, (Typ.var param_name)::args)
                      | None ->
                          let new_name = sprintf "a%d" n in
                          (n+1, FoldInfo.extend new_name arg arg map, (Typ.var new_name)::args)
            )
#if OCAML_VERSION > (4, 02, 2)
            (match cd.pcd_args with Pcstr_tuple tt -> tt | Pcstr_record _ -> assert false)
#else
            cd.pcd_args
#endif
            (n, acc_map,[])
          in
#if OCAML_VERSION > (4, 02, 2)
          let new_args = Pcstr_tuple new_args in
#endif
          (n, map2, { cd with pcd_args = new_args } :: cs)
      )
      ctors
      (0, FoldInfo.empty, [])
      |> (fun (_, mapa, cs) -> mapa, {tdecl with ptype_kind = Ptype_variant cs})
  in
  (* now we need to add some parameters if we collected ones *)
  let functor_typ, typ_to_add =
    if FoldInfo.is_empty mapa
    then
      let fmap_for_typ = prepare_fmap ~loc full_t in
      let () = print_endline "fmap prepared" in
      full_t, (*(str_type_ ~loc Nonrecursive [full_t]) ::*) (prepare_distribs ~loc full_t fmap_for_typ)
    else
      let functor_typ =
        let extra_params = FoldInfo.map mapa
          ~f:(fun fi -> (Ast_helper.Typ.var fi.FoldInfo.param_name, Asttypes.Invariant))
        in
        let open Location in
        {full_t with ptype_params = full_t.ptype_params @ extra_params;
                     ptype_name = { full_t.ptype_name with txt = "g" ^ full_t.ptype_name.txt }}
      in
      let fmap_for_typ = prepare_fmap ~loc functor_typ in
      functor_typ, ([fmap_for_typ] @ (prepare_distribs ~loc functor_typ fmap_for_typ))
  in
(*      let non_logic_typ =
        let alias_desc =
          let old_params = List.map fst tdecl.ptype_params  in
          let extra_params = FoldInfo.map ~f:(fun {FoldInfo.rtyp} -> rtyp)  mapa in
          Ptyp_constr (Location.mknoloc (Longident.Lident functor_typ.ptype_name.Asttypes.txt), old_params @ extra_params)
        in
        str_type_ ~loc Recursive
          [ { tdecl with ptype_kind = Ptype_abstract
            ; ptype_manifest = Some { ptyp_loc = Location.none; ptyp_attributes = []; ptyp_desc = alias_desc}
            } ]
      in*)
    [ str_type_ ~loc Recursive [functor_typ]
    ] @ typ_to_add

let has_to_gen_attr (xs: attributes) =
  try let _ = List.find (fun (name,_) -> name.Location.txt = "put_distrib_here") xs in
      true
  with Not_found -> false

let main_mapper =
  let wrap_tydecls loc ts =
    let f tdecl =
      match tdecl.ptype_kind with
      | Ptype_variant cs when tdecl.ptype_manifest = None && has_to_gen_attr tdecl.ptype_attributes ->
          revisit_adt ~loc tdecl cs
      | _ -> failwith "Only variant types without manifest are supported"
    in
    List.flatten (List.map f ts)
  in

  { Ast_mapper.default_mapper with
    structure = fun self ss ->
      let f si = match si.pstr_desc with
#if OCAML_VERSION > (4, 02, 2)
      | Pstr_type (_,tydecls) ->
#else
      | Pstr_type tydecls ->
#endif
        wrap_tydecls si.pstr_loc tydecls
      | x -> [si]
      in
      List.flatten (List.map f ss)
  }

let process x =
  main_mapper.structure main_mapper x
