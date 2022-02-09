open Printf
open Ast_mapper
open Ast_helper
open Parsetree
open Location

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

let extract_name typ =
    match typ.ptyp_desc with
    | Ptyp_var s -> s
    | _ ->
      Buffer.clear Format.stdbuf;
      Pprintast.core_type Format.str_formatter typ;
      failwith (sprintf "Don't know what to do with %s" (Format.flush_str_formatter ()))

let extract_names = List.map extract_name

let str_type_ = Ast_helper.Str.type_

let nolabel =
      Asttypes.Nolabel

let get_param_names pcd_args =
  match pcd_args with
  | Pcstr_tuple pcd_args -> extract_names pcd_args
  | _ -> failwith "not implemented"

let lower_lid lid = Location.{lid with txt = Util.mangle_construct_name lid.Location.txt }

let prepare_distribs_for_FAT ~loc tdecl fmap_decl =
  let open Location in
  let open Longident in
  let type_name = tdecl.ptype_name.txt in
  let module_str = "For_" ^ type_name in
  let gen_module_str = mknoloc module_str in
  let gen_module_opt_str = mknoloc @@ Some module_str in
  let distrib_lid = mknoloc Longident.(Ldot (Lident gen_module_str.txt, "distrib")) in
  [ Str.module_ @@ Mb.mk gen_module_opt_str @@
      Mod.(apply (ident (mknoloc @@ Lident (let n = List.length tdecl.ptype_params in if n = 1 then "Fmap" else sprintf "Fmap%d" n))) @@ structure
        [ fmap_decl
        ; str_type_ Recursive ~loc [ Type.mk ~params:tdecl.ptype_params
            ~kind:Ptype_abstract
            ~manifest:(Typ.constr (mknoloc @@ Lident tdecl.ptype_name.txt) @@ List.map fst tdecl.ptype_params)
            (mknoloc "t") ]
        ])
  ;
  match tdecl.ptype_kind with
  | Ptype_variant constructors ->
    Str.value Recursive @@ List.map (fun {pcd_name; pcd_args} ->
        let names =
          let[@warning "-8"]  Pcstr_tuple xs = pcd_args in
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
      Vb.mk ~attrs:[Attr.mk (mknoloc "service_function") (Parsetree.PStr [])] (Pat.var @@ lower_lid pcd_name)
        (match names with
        | [] -> [%expr fun () -> [%e body]]
        | names -> List.fold_right (fun name acc -> Exp.fun_ nolabel None (Pat.var @@ mknoloc name) acc) names body)
    ) constructors
  | Ptype_record fields ->
    fields
    |> List.map (fun { pld_name } ->
      let name = pld_name.txt in
      let lid  = mknoloc @@ Longident.Lident name in
      lid, Exp.ident lid)
    |> fun b -> [%expr inj [%e Exp.apply (Exp.ident distrib_lid) [nolabel, Exp.record b None] ] ]
    |> List.fold_right (function | { pld_name } -> Exp.fun_ nolabel None Pat.(var @@ mknoloc pld_name.txt)) fields
    |> Vb.mk ~attrs:[Attr.mk (mknoloc "service_function") (Parsetree.PStr [])] (Pat.var @@ mknoloc @@ sprintf "ctor_%s" type_name)
    |> fun vb -> Str.value Nonrecursive [vb]
  | Ptype_abstract
  | Ptype_open -> failwith "Not supported"
    ]

let prepare_distribs ~loc tdecl fmap_decl =
  let open Location in
  let open Longident in
  let open Exp in
  if List.length tdecl.ptype_params > 0
  then prepare_distribs_for_FAT ~loc tdecl fmap_decl
  else match  tdecl.ptype_kind with
     | (Ptype_abstract|Ptype_open|Ptype_record _) -> failwith "Not supported"
     | Ptype_variant constructors ->
       constructors |>
       List.map (fun {pcd_name} ->
         Vb.mk ~attrs:[Attr.mk (mknoloc "service_function") (Parsetree.PStr [])]
               (Pat.var @@ lower_lid pcd_name)
               ([%expr fun () -> !! [%e construct (mknoloc (Lident pcd_name.txt)) None]])) |>
       List.map (fun vb -> Str.value Nonrecursive [vb])


let prepare_fmap ~loc tdecl useGT =
  if useGT
    then [%stri let[@service_function] fmap eta = GT.gmap t eta]
    else

    let open Location in
    let open Ast_helper in
    let param_names = extract_names (List.map fst tdecl.ptype_params) in
    match tdecl.ptype_kind with
    | Ptype_variant constructors ->
      let cases = constructors |> List.map (fun {pcd_name; pcd_args} ->
        let argnames = get_param_names pcd_args in
        let cname = pcd_name.txt in
        let clid = mknoloc @@ Longident.Lident cname in
        let make_f_expr name = Exp.ident @@ mknoloc @@ Longident.Lident ("f"^name) in
        let pc_lhs, pc_rhs =
          let wrap_one_arg typname new_name =
            Exp.(apply (make_f_expr typname) [nolabel, ident @@ mknoloc @@ Longident.Lident new_name] )
          in
          let get_pat_name i name = sprintf "%s_%d" name i in
          match argnames with
          | [] -> (None, None)
          | [s] -> Some ([], Ast_helper.Pat.var (mknoloc s)), (Some (wrap_one_arg s s))
          | ___ ->
              Some ([], Ast_helper.Pat.(tuple @@ List.mapi (fun n name -> var (mknoloc @@ get_pat_name n name)) argnames)),
              Some (Exp.tuple @@ List.mapi (fun n name -> wrap_one_arg name @@ get_pat_name n name) argnames)
        in
        let pc_lhs = Ast_helper.Pat.construct clid pc_lhs in
        let pc_rhs = Exp.construct clid pc_rhs in
        let pc_guard = None in
        { pc_rhs; pc_lhs; pc_guard }
      ) in
      [%stri let[@service_function] rec fmap = [%e
        List.fold_right (function
        | name -> Exp.fun_ nolabel None Pat.(var @@ mknoloc ("f"^name))
        ) param_names (Exp.function_ cases) ] ]
    | Ptype_record fields ->
      let pattern = fields |> List.map (fun { pld_name } ->
        let name = pld_name.txt in
        let lid  = mknoloc @@ Longident.Lident name in
        lid, Pat.var @@ mknoloc name)
        |> fun b -> Pat.record b Closed in
      let body = fields |> List.map (fun { pld_type; pld_name } ->
        let type_name = Exp.ident @@ mknoloc @@ Longident.Lident ("f" ^ extract_name pld_type) in
        let lid       = mknoloc @@ Longident.Lident pld_name.txt in
        lid, Exp.apply type_name [nolabel, Exp.ident lid])
        |> fun b -> Exp.record b None
        |> Exp.fun_ Asttypes.Nolabel None pattern in

      [%stri let[@service_function] rec fmap = [%e
        List.fold_right (fun name ->
          Exp.fun_ nolabel None Pat.(var @@ mknoloc ("f"^name))
        ) param_names body ] ]
  | Ptype_abstract
  | Ptype_open -> failwith "Not supported"




let revisit_type loc tdecl useGT =
  let tdecl = {tdecl with ptype_attributes =
    List.filter (fun a -> a.attr_name.Location.txt <> "put_distrib_here") tdecl.ptype_attributes }
  in

  (* convert type to fully-abstract one *)
  let mapa, full_t =
    match tdecl.ptype_kind with
    | Ptype_variant ctors ->
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
              (match cd.pcd_args with Pcstr_tuple tt -> tt | Pcstr_record _ -> assert false)
              (n, acc_map,[])
            in
            let new_args = Pcstr_tuple new_args in
            (n, map2, { cd with pcd_args = new_args } :: cs)
        )
        ctors
        (0, FoldInfo.empty, [])
        |> (fun (_, mapa, cs) -> mapa, {tdecl with ptype_kind = Ptype_variant cs})
    | Ptype_record fields ->
      List.fold_right (fun field (n, map, args) ->
        let typ = field.pld_type in
        let upd_field typ = { field with pld_type = typ } in
        match typ with
        | [%type: _] -> assert false
        | {ptyp_desc = Ptyp_var s; _} -> (n, map, field :: args)
        | arg ->
            match FoldInfo.param_for_rtyp arg map with
            | Some {param_name; } -> (n, map, upd_field (Typ.var param_name) :: args)
            | None ->
                let new_name = sprintf "a%d" n in
                (n+1, FoldInfo.extend new_name arg arg map, upd_field (Typ.var new_name) :: args)
        )
      fields (0, FoldInfo.empty, [])
      |>  (fun (_, mapa, fields) -> mapa, {tdecl with ptype_kind = Ptype_record fields})
  | Ptype_abstract
  | Ptype_open -> failwith "Not supported"
  in

  (* now we need to add some parameters if we collected ones *)
  let functor_typ, typ_to_add =
    let open (struct
      [%%if ocaml_version < (4, 11, 0)]
      let make_simple_arg x = (x, Asttypes.Invariant)
      [%%else ]
      let make_simple_arg x = (x, (Asttypes.NoVariance, Asttypes.NoInjectivity))
      [%%endif]
      end)
    in
    let full_t = {full_t with ptype_name = { full_t.ptype_name with txt = "g" ^ full_t.ptype_name.txt }} in
    let result_type =
      if FoldInfo.is_empty mapa then full_t else
        let extra_params = FoldInfo.map mapa
          ~f:(fun fi -> (make_simple_arg @@ Ast_helper.Typ.var fi.FoldInfo.param_name)) in
        {full_t with ptype_params = full_t.ptype_params @ extra_params} in

      let fmap_for_typ = prepare_fmap ~loc result_type useGT in
      result_type, (prepare_distribs ~loc result_type fmap_for_typ)
  in
    let gt_attribute =
      if useGT
        then [Attr.mk (mknoloc "deriving") (PStr [Str.eval [%expr gt ~options:{ show; gmap }]])]
        else [] in
    let functor_typ = { functor_typ with ptype_attributes = gt_attribute @ functor_typ.ptype_attributes } in
    [ str_type_ ~loc Recursive [functor_typ]
    ] @ typ_to_add

let has_to_gen_attr (xs: attributes) =
  try let _ = List.find (fun a -> a.attr_name.Location.txt = "put_distrib_here") xs in
      true
  with Not_found -> false

let main_mapper useGT =
  let wrap_tydecls loc ts =
    let f tdecl =
      match tdecl.ptype_kind with
      | Ptype_variant _
      | Ptype_record  _ when tdecl.ptype_manifest = None && has_to_gen_attr tdecl.ptype_attributes ->
          revisit_type loc tdecl useGT
      | _ -> failwith "Only variant types without manifest are supported"
    in
    List.flatten (List.map f ts)
  in

  { Ast_mapper.default_mapper with
    structure = fun self ss ->
      let f si = match si.pstr_desc with
      | Pstr_type (_,tydecls) ->
        wrap_tydecls si.pstr_loc tydecls
      | x -> [si]
      in
      List.flatten (List.map f ss)
  }

let process useGT x =
  (main_mapper useGT).structure (main_mapper useGT) x
