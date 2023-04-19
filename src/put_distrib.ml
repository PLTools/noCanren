open Printf
open Ast_mapper
open Ast_helper
open Parsetree
open Location
module TypeNameMap = Map.Make (String)

module FoldInfo : sig
  type t

  type item =
    { param_name : string
    ; info : [ `Self | `Concrete of core_type * core_type ]
    }

  val empty : t
  val is_empty : t -> bool
  val param_for_rtyp : core_type -> t -> item option
  val extend : string -> [ `Self | `Concrete of core_type * core_type ] -> t -> t
  val map : f:(item -> 'a) -> t -> 'a list
  val pp : Format.formatter -> t -> unit
  val pp_item : Format.formatter -> item -> unit
  val substitute : t -> self:core_type -> core_type list -> core_type list
end = struct
  type item =
    { param_name : string
    ; info : [ `Self | `Concrete of core_type * core_type ]
    }

  exception ItemFound of item

  type t = item list

  let pp_item ppf = function
    | { param_name; info = `Self } -> Format.fprintf ppf "%s: self" param_name
    | { param_name; info = `Concrete (ltyp, rtyp) } ->
      let open Format in
      fprintf
        ppf
        "%s: %a -> %a"
        param_name
        Pprintast.core_type
        ltyp
        Pprintast.core_type
        rtyp
  ;;

  let pp ppf xs =
    let open Format in
    pp_print_list pp_item ppf xs
  ;;

  let param_for_rtyp typ ts =
    let typ_repr = Format.asprintf "%a" Pprintast.core_type typ in
    try
      List.iter
        (function
         | { info = `Self } -> ()
         | { info = `Concrete (ltyp, rtyp) } as i ->
           let new_repr = Format.asprintf "%a" Pprintast.core_type rtyp in
           if new_repr = typ_repr then raise (ItemFound i))
        ts;
      None
    with
    | ItemFound i -> Some i
  ;;

  let map ~f (xs : t) = List.map f xs
  let empty = []
  let is_empty : t -> bool = ( = ) []

  let extend param_name info ts =
    (*      printf "extending by `%s`\n%!" param_name;*)
    if param_name = "self" && List.exists (fun { param_name } -> param_name = "self") ts
    then ts
    else { param_name; info } :: ts
  ;;

  let substitute t ~self args = args
end

let str_type_ = Ast_helper.Str.type_

let lower_lid lid =
  Location.{ lid with txt = Util.mangle_construct_name lid.Location.txt }
;;

module Old_OCanren = struct
  let extract_name typ =
    match typ.ptyp_desc with
    | Ptyp_var s -> s
    | _ ->
      Buffer.clear Format.stdbuf;
      Pprintast.core_type Format.str_formatter typ;
      failwith (sprintf "Don't know what to do with %s" (Format.flush_str_formatter ()))
  ;;

  let extract_names = List.map extract_name

  let get_param_names pcd_args =
    match pcd_args with
    | Pcstr_tuple pcd_args -> extract_names pcd_args
    | _ -> failwith "not implemented"
  ;;

  let nolabel = Asttypes.Nolabel

  let prepare_distribs_for_FAT ~loc ~type_name tdecl fmap_decl =
    let open Location in
    let open Longident in
    let module_str = Util.ctor_module_prefix ^ tdecl.ptype_name.txt in
    let gen_module_str = mknoloc module_str in
    let gen_module_opt_str = mknoloc @@ Some module_str in
    let distrib_lid = mknoloc Longident.(Ldot (Lident gen_module_str.txt, "distrib")) in
    [ (Str.module_
       @@ Mb.mk gen_module_opt_str
       @@ Mod.(
            apply
              (ident
                 (mknoloc
                  @@ Lident
                       (let n = List.length tdecl.ptype_params in
                        if n = 1 then "Fmap" else sprintf "Fmap%d" n)))
            @@ structure
                 [ fmap_decl
                 ; str_type_
                     Recursive
                     ~loc
                     [ Type.mk
                         ~params:tdecl.ptype_params
                         ~kind:Ptype_abstract
                         ~manifest:
                           (Typ.constr (mknoloc @@ Lident tdecl.ptype_name.txt)
                            @@ List.map fst tdecl.ptype_params)
                         (mknoloc "t")
                     ]
                 ]))
    ; (match tdecl.ptype_kind with
       | Ptype_variant constructors ->
         Str.value Recursive
         @@ List.map
              (fun { pcd_name; pcd_args } ->
                let names =
                  let[@warning "-8"] (Pcstr_tuple xs) = pcd_args in
                  List.mapi (fun n _ -> Printf.sprintf "x__%d" n) xs
                in
                let open Exp in
                let body =
                  let constr_itself = construct @@ mknoloc (Lident pcd_name.txt) in
                  match names with
                  | [] -> constr_itself None
                  | [ one ] -> constr_itself (Some (ident @@ mknoloc (Lident one)))
                  | xs ->
                    (* construct tuple here *)
                    constr_itself
                      (Some
                         (tuple
                          @@ List.map (fun name -> ident @@ mknoloc (Lident name)) xs))
                in
                let body =
                  [%expr inj [%e Exp.apply (Exp.ident distrib_lid) [ nolabel, body ]]]
                in
                Vb.mk
                  ~attrs:[ Attr.mk (mknoloc "service_function") (Parsetree.PStr []) ]
                  (Pat.var @@ lower_lid pcd_name)
                  (match names with
                   | [] -> [%expr fun () -> [%e body]]
                   | names ->
                     List.fold_right
                       (fun name acc ->
                         Exp.fun_ nolabel None (Pat.var @@ mknoloc name) acc)
                       names
                       body))
              constructors
       | Ptype_record fields ->
         fields
         |> List.map (fun { pld_name } ->
              let name = pld_name.txt in
              let lid = mknoloc @@ Longident.Lident name in
              lid, Exp.ident lid)
         |> fun b ->
         [%expr inj [%e Exp.apply (Exp.ident distrib_lid) [ nolabel, Exp.record b None ]]]
         |> List.fold_right
              (function
               | { pld_name } -> Exp.fun_ nolabel None Pat.(var @@ mknoloc pld_name.txt))
              fields
         |> Vb.mk
              ~attrs:[ Attr.mk (mknoloc "service_function") (Parsetree.PStr []) ]
              (Pat.var @@ mknoloc @@ sprintf "ctor_%s" type_name)
         |> fun vb -> Str.value Nonrecursive [ vb ]
       | Ptype_abstract | Ptype_open -> Util.fail_loc loc "Not supported")
    ]
  ;;

  let prepare_distribs ~loc ~type_name tdecl fmap_decl =
    let open Location in
    let open Longident in
    let open Exp in
    if List.length tdecl.ptype_params > 0
    then prepare_distribs_for_FAT ~loc ~type_name tdecl fmap_decl
    else (
      match tdecl.ptype_kind with
      | Ptype_abstract | Ptype_open | Ptype_record _ -> Util.fail_loc loc "Not supported"
      | Ptype_variant constructors ->
        constructors
        |> List.map (fun { pcd_name } ->
             Vb.mk
               ~attrs:[ Attr.mk (mknoloc "service_function") (Parsetree.PStr []) ]
               (Pat.var @@ lower_lid pcd_name)
               [%expr fun () -> !![%e construct (mknoloc (Lident pcd_name.txt)) None]])
        |> List.map (fun vb -> Str.value Nonrecursive [ vb ]))
  ;;

  let prepare_fmap ~loc tdecl useGT =
    if useGT
    then [%stri let[@service_function] fmap eta = GT.gmap t eta]
    else
      let open Location in
      let open Ast_helper in
      let param_names = extract_names (List.map fst tdecl.ptype_params) in
      match tdecl.ptype_kind with
      | Ptype_variant constructors ->
        let cases =
          constructors
          |> List.map (fun { pcd_name; pcd_args } ->
               let argnames = get_param_names pcd_args in
               let cname = pcd_name.txt in
               let clid = mknoloc @@ Longident.Lident cname in
               let make_f_expr name =
                 Exp.ident @@ mknoloc @@ Longident.Lident ("f" ^ name)
               in
               let pc_lhs, pc_rhs =
                 let wrap_one_arg typname new_name =
                   Exp.(
                     apply
                       (make_f_expr typname)
                       [ nolabel, ident @@ mknoloc @@ Longident.Lident new_name ])
                 in
                 let get_pat_name i name = sprintf "%s_%d" name i in
                 match argnames with
                 | [] -> None, None
                 | [ s ] ->
                   Some ([], Ast_helper.Pat.var (mknoloc s)), Some (wrap_one_arg s s)
                 | ___ ->
                   ( Some
                       ( []
                       , Ast_helper.Pat.(
                           tuple
                           @@ List.mapi
                                (fun n name -> var (mknoloc @@ get_pat_name n name))
                                argnames) )
                   , Some
                       (Exp.tuple
                        @@ List.mapi
                             (fun n name -> wrap_one_arg name @@ get_pat_name n name)
                             argnames) )
               in
               let pc_lhs = Ast_helper.Pat.construct clid pc_lhs in
               let pc_rhs = Exp.construct clid pc_rhs in
               let pc_guard = None in
               { pc_rhs; pc_lhs; pc_guard })
        in
        [%stri
          let[@service_function] rec fmap =
            [%e
              List.fold_right
                (function
                 | name -> Exp.fun_ nolabel None Pat.(var @@ mknoloc ("f" ^ name)))
                param_names
                (Exp.function_ cases)]
          ;;]
      | Ptype_record fields ->
        let pattern =
          fields
          |> List.map (fun { pld_name } ->
               let name = pld_name.txt in
               let lid = mknoloc @@ Longident.Lident name in
               lid, Pat.var @@ mknoloc name)
          |> fun b -> Pat.record b Closed
        in
        let body =
          fields
          |> List.map (fun { pld_type; pld_name } ->
               let type_name =
                 Exp.ident @@ mknoloc @@ Longident.Lident ("f" ^ extract_name pld_type)
               in
               let lid = mknoloc @@ Longident.Lident pld_name.txt in
               lid, Exp.apply type_name [ nolabel, Exp.ident lid ])
          |> fun b -> Exp.record b None |> Exp.fun_ Asttypes.Nolabel None pattern
        in
        [%stri
          let[@service_function] rec fmap =
            [%e
              List.fold_right
                (fun name -> Exp.fun_ nolabel None Pat.(var @@ mknoloc ("f" ^ name)))
                param_names
                body]
          ;;]
      | Ptype_abstract | Ptype_open -> Util.fail_loc loc "Not supported"
  ;;
end

let prepare_distribs_new ~loc ~type_name tdecl =
  let for_record pat_name add_constr labs =
    let open Longident in
    let r =
      Exp.record
        (List.map
           (fun { pld_name } ->
             let lid = mkloc (Lident pld_name.txt) pld_name.loc in
             lid, Exp.ident lid)
           labs)
        None
    in
    [%stri
      let [%p pat_name] =
        [%e
          List.fold_right
            (fun { pld_name } acc ->
              [%expr
                fun [%p Pat.var @@ mkloc (Printf.sprintf "%s" pld_name.txt) loc] ->
                  [%e acc]])
            labs
            [%expr OCanren.inj [%e add_constr r]]]
      ;;]
  in
  match tdecl.ptype_kind with
  | Ptype_abstract | Ptype_open ->
    Util.fail_loc loc "Not supported %s %d" __FILE__ __LINE__
  | Ptype_record labs ->
    [ for_record
        (Pat.var (mkloc (Printf.sprintf "ctor_%s" type_name) tdecl.ptype_name.loc))
        (fun r -> r)
        labs
    ]
  | Ptype_variant cs ->
    let open Exp in
    let open Longident in
    cs
    |> List.map (fun { pcd_name; pcd_args } ->
         let construct_expr = construct (mknoloc (Lident pcd_name.txt)) in
         match pcd_args with
         | Pcstr_record labs ->
           for_record
             (Pat.var (lower_lid pcd_name))
             (fun r -> construct_expr (Some r))
             labs
         | Pcstr_tuple [] ->
           (* There we need to add extra unit argument *)
           [%stri
             let [%p Pat.var (lower_lid pcd_name)] =
              fun () -> OCanren.inj [%e construct_expr None]
            ;;]
         | Pcstr_tuple ts ->
           let ns = List.mapi (fun n _ -> n) ts in
           let add_abs =
             List.fold_right
               (fun n acc next ->
                 [%expr
                   fun [%p Pat.var @@ mkloc (Printf.sprintf "x%d" n) loc] -> [%e acc next]])
               ns
               (fun x -> x)
           in
           let add_args =
             construct_expr
             @@ Option.some
             @@ Exp.tuple
             @@ List.map
                  (fun x -> Exp.ident (mknoloc (Lident (Printf.sprintf "x%d" x))))
                  ns
           in
           Str.value
             Nonrecursive
             [ Vb.mk
                 ~attrs:[ Attr.mk (mknoloc "service_function") (Parsetree.PStr []) ]
                 (Pat.var @@ lower_lid pcd_name)
                 (add_abs [%expr OCanren.inj [%e add_args]])
             ])
;;

let revisit_type ~params rec_flg loc tdecl =
  let tdecl =
    { tdecl with
      ptype_attributes =
        List.filter
          (fun a -> a.attr_name.Location.txt <> "put_distrib_here")
          tdecl.ptype_attributes
    }
  in
  (* convert type to fully-abstract one *)
  let abstracting_internal_type ~selfname typ (n, map, args) =
    match typ with
    | [%type: _] -> Util.fail_loc typ.ptyp_loc "Wildcards are not supported"
    | { ptyp_desc = Ptyp_var s; _ } -> n, map, typ :: args
    | { ptyp_desc = Ptyp_constr ({ txt = Lident s }, _) } when s = selfname ->
      let new_name = "self" in
      n, FoldInfo.extend new_name `Self map, Typ.var new_name :: args
    | arg ->
      (match FoldInfo.param_for_rtyp arg map with
       | Some { param_name } -> n, map, Typ.var param_name :: args
       | None ->
         let new_name = sprintf "a%d" n in
         ( n + 1
         , FoldInfo.extend new_name (`Concrete (arg, arg)) map
         , Typ.var new_name :: args ))
  in
  let abstr_info =
    let () =
      match tdecl.ptype_manifest with
      | Some _ -> Util.fail_loc tdecl.ptype_loc "No manifest required"
      | None -> ()
    in
    let mapa, new_kind =
      match tdecl.ptype_kind with
      | Ptype_abstract | Ptype_open -> Util.fail_loc loc "Not supported"
      | Ptype_variant ctors ->
        let _, mapa, kind =
          List.fold_right
            (fun cd (n, acc_map, cs) ->
              let acc = n, acc_map, [] in
              match cd.pcd_args with
              | Pcstr_tuple tt ->
                let n, map2, new_args =
                  List.fold_right
                    (abstracting_internal_type ~selfname:tdecl.ptype_name.txt)
                    tt
                    acc
                in
                let new_args = Pcstr_tuple new_args in
                n, map2, { cd with pcd_args = new_args } :: cs
              | Pcstr_record lds ->
                let typs = List.map (fun ldt -> ldt.pld_type) lds in
                let n, map2, new_args =
                  List.fold_right
                    (abstracting_internal_type ~selfname:tdecl.ptype_name.txt)
                    typs
                    acc
                in
                let new_args =
                  Pcstr_record
                    (List.map2 (fun ld t -> { ld with pld_type = t }) lds new_args)
                in
                n, map2, { cd with pcd_args = new_args } :: cs)
            ctors
            (0, FoldInfo.empty, [])
        in
        mapa, Ptype_variant kind
      | Ptype_record fields ->
        let _, mapa, kind =
          List.fold_right
            (fun field (n, map, args) ->
              let typ = field.pld_type in
              let upd_field typ = { field with pld_type = typ } in
              match typ with
              | [%type: _] -> assert false
              | { ptyp_desc = Ptyp_var s; _ } -> n, map, field :: args
              | { ptyp_desc = Ptyp_constr ({ txt = Lident s }, _) }
                when s = tdecl.ptype_name.txt ->
                let new_name = "self" in
                ( n
                , FoldInfo.extend new_name `Self map
                , upd_field (Typ.var new_name) :: args )
              | arg ->
                (match FoldInfo.param_for_rtyp arg map with
                 | Some { param_name } -> n, map, upd_field (Typ.var param_name) :: args
                 | None ->
                   let new_name = sprintf "a%d" n in
                   ( n + 1
                   , FoldInfo.extend new_name (`Concrete (arg, arg)) map
                   , upd_field (Typ.var new_name) :: args )))
            fields
            (0, FoldInfo.empty, [])
        in
        mapa, Ptype_record kind
    in
    let ptype_manifest =
      match params.Util.reexport_path with
      | None -> None
      | Some prefix ->
        (match Longident.unflatten (prefix @ [ tdecl.ptype_name.txt ]) with
         | None -> None
         | Some lid ->
           if new_kind = tdecl.ptype_kind
           then
             (* Indeed fully abstract *)
             Option.some
             @@ Ast_helper.Typ.constr
                  (Location.mknoloc lid)
                  (List.map fst tdecl.ptype_params)
           else None)
    in
    (*
    Format.printf "(* mapa = %a *)\n%!" FoldInfo.pp mapa;
     *)
    if FoldInfo.is_empty mapa
    then `AlreadyFull { tdecl with ptype_manifest }
    else (
      let make_simple_arg x = x, (Asttypes.NoVariance, Asttypes.NoInjectivity) in
      let make_to_types xname =
        let full_t =
          { tdecl with
            ptype_manifest
          ; ptype_kind = new_kind
          ; ptype_name = Location.mkloc xname tdecl.ptype_name.loc
          ; ptype_params =
              tdecl.ptype_params
              @ FoldInfo.map mapa ~f:(fun fi ->
                  make_simple_arg @@ Ast_helper.Typ.var fi.FoldInfo.param_name)
          }
        in
        let extra_params =
          FoldInfo.map mapa ~f:(fun fi -> Ast_helper.Typ.var fi.FoldInfo.param_name)
        in
        let spec_typ =
          Typ.alias
            (Typ.constr
               (Location.mknoloc (Longident.Lident full_t.ptype_name.txt))
               (FoldInfo.substitute
                  mapa
                  ~self:[%type: 'self]
                  (List.map fst tdecl.ptype_params @ extra_params)))
            "self"
        in
        let alias = { tdecl with ptype_manifest = Some spec_typ } in
        full_t, Str.type_ Recursive [ alias ], spec_typ
      in
      `Extra make_to_types)
  in
  let add_gt_attribute =
    if params.Util.useGT
    then (
      let attr =
        Attr.mk
          (mknoloc "deriving")
          (PStr [ Str.eval [%expr gt ~options:{ show; fmt; gmap }] ])
      in
      fun tdecl -> { tdecl with ptype_attributes = attr :: tdecl.ptype_attributes })
    else Fun.id
  in
  match params.Util.gen_info, abstr_info with
  | Util.Old_OCanren, _ ->
    let full_t, extra =
      match abstr_info with
      | `AlreadyFull full_t -> full_t, []
      | `Extra f ->
        let full_t, extra, _ = f ("g" ^ tdecl.ptype_name.txt) in
        full_t, [ extra ]
    in
    let fmap_for_typ = Old_OCanren.prepare_fmap ~loc full_t params.Util.useGT in
    List.concat
      [ [ str_type_ ~loc Recursive [ full_t ] ]
      ; extra
      ; Old_OCanren.prepare_distribs
          ~loc
          ~type_name:tdecl.ptype_name.txt
          full_t
          fmap_for_typ
      ]
  | Only_injections, `AlreadyFull full_t ->
    let full_t =
      { full_t with ptype_name = Location.mknoloc ("g" ^ tdecl.ptype_name.txt) }
    in
    str_type_ ~loc Recursive [ full_t ]
    :: prepare_distribs_new ~loc ~type_name:tdecl.ptype_name.txt full_t
  | Distribs, `Extra f | Only_injections, `Extra f ->
    (* Format.printf "(* %s %d *)\n%!" __FILE__ __LINE__; *)
    let full_t, extra, spec = f ("g" ^ tdecl.ptype_name.txt) in
    List.concat
      [ [ str_type_ ~loc Recursive [ full_t ] ]
      ; prepare_distribs_new ~loc ~type_name:tdecl.ptype_name.txt full_t
      ; (match params.Util.reexport_path with
         | None -> []
         | Some prefix ->
           (* Going to generate unsafe cast *)
           let tfrom =
             Typ.constr
               (Location.mknoloc
                  (Longident.unflatten (prefix @ [ tdecl.ptype_name.txt ]) |> Option.get))
               (List.map fst tdecl.ptype_params)
           in
           (* [%str external cast : [%t tfrom] -> [%t spec] = "%identity"] *)
           let p_to_ground =
             Pat.var (Location.mknoloc (sprintf "%s_to_ground" tdecl.ptype_name.txt))
           in
           let p_from_ground =
             Pat.var (Location.mknoloc (sprintf "%s_from_ground" tdecl.ptype_name.txt))
           in
           [%str
             let ([%p p_to_ground] : [%t tfrom] -> [%t spec]) = Obj.magic
             let ([%p p_from_ground] : [%t spec] -> [%t tfrom]) = Obj.magic])
      ]
  | Distribs, `AlreadyFull full_t ->
    (* Format.printf "(* %s %d *)\n%!" __FILE__ __LINE__; *)
    let open Ast_helper in
    let abstract_t =
      { full_t with ptype_name = Location.mkloc "t" tdecl.ptype_name.loc }
    in
    let t2 =
      { tdecl with
        ptype_name = Location.mknoloc "ground"
      ; ptype_kind = Ptype_abstract
      ; ptype_manifest =
          Option.some
          @@ Ast_helper.Typ.constr
               (Location.mknoloc (Longident.Lident "t"))
               (List.map fst tdecl.ptype_params)
      }
    in
    let make ~mname t1 t2 ~creators =
      let type_synonym =
        let open Longident in
        str_type_
          ~loc
          Nonrecursive
          [ Ast_helper.Type.mk
              ~params:t1.ptype_params
              ~kind:t1.ptype_kind
              ~manifest:
                (Typ.constr
                   (Location.mknoloc @@ Ldot (Lident mname, "t"))
                   (List.map fst t1.ptype_params))
              (Location.mkloc ("g" ^ mname) tdecl.ptype_name.loc)
          ]
      in
      Str.module_
        (Mb.mk (Location.mknoloc (Some mname))
         @@ Mod.structure
              [ Str.extension
                  ~loc
                  ( Location.mknoloc "distrib"
                  , PStr (str_type_ ~loc Nonrecursive [ t1 ] :: t2) )
              ])
      :: type_synonym
      :: creators
    in
    let mname =
      String.mapi
        (fun i -> if i = 0 then Char.uppercase_ascii else Fun.id)
        tdecl.ptype_name.txt
    in
    let exposed_creators =
      match tdecl.ptype_kind with
      | Ptype_abstract | Ptype_open -> failwith "not supported"
      | Ptype_variant cds ->
        List.map
          (fun { pcd_name } ->
            let func = lower_lid pcd_name in
            Str.value Nonrecursive
            @@ [ Vb.mk
                   (Pat.var func)
                   (Exp.ident
                      (Location.mknoloc (Longident.Ldot (Lident mname, func.txt))))
               ])
          cds
      | Ptype_record _ -> assert false
    in
    make
      (add_gt_attribute abstract_t)
      [ Ast_helper.Str.type_ rec_flg [ t2 ] ]
      ~mname
      ~creators:exposed_creators
  | Only_distribs, _ -> failwith "Unexpected type transtation mode"
;;

(* [ Ast_helper.Str.extension
        ~loc
        ( Location.mknoloc "distrib"
        , PStr (str_type_ ~loc Recursive [ functor_typ ] :: typ_to_add) )
    ] *)

let has_to_gen_attr (xs : attributes) =
  try
    let _ = List.find (fun a -> a.attr_name.Location.txt = "put_distrib_here") xs in
    true
  with
  | Not_found -> false
;;

let put_distrib ~params rec_flg loc tdecl =
  let gt_params =
    Attr.mk
      (mknoloc "deriving")
      (PStr [ Str.eval [%expr gt ~options:{ show; fmt; gmap }] ])
  in
  let tdecl =
    { tdecl with
      ptype_attributes =
        gt_params
        :: List.filter
             (fun a -> a.attr_name.Location.txt <> "put_distrib_here")
             tdecl.ptype_attributes
    }
  in
  [ Ast_helper.Str.extension
      ~loc
      (Location.mknoloc "distrib", PStr [ str_type_ ~loc Recursive [ tdecl ] ])
  ]
;;

let mk_record_constr ~loc type_name fields =
  fields
  |> List.map (fun { pld_name } ->
       let name = pld_name.txt in
       let lid = mknoloc @@ Longident.Lident name in
       lid, Exp.ident lid)
  |> fun b ->
  [%expr inj [%e Exp.record b None]]
  |> List.fold_right
       (function
        | { pld_name } -> Exp.fun_ Nolabel None Pat.(var @@ mknoloc pld_name.txt))
       fields
  |> Vb.mk
       ~attrs:[ Attr.mk (mknoloc "service_function") (Parsetree.PStr []) ]
       (Pat.var @@ mknoloc @@ sprintf "ctor_%s" type_name)
  |> fun vb -> Str.value Nonrecursive [ vb ]
;;

let translate_type ~params rec_flg loc tdecl =
  let open Util in
  match params.gen_info with
  | Only_distribs ->
    (match tdecl.ptype_kind with
     | Ptype_abstract | Ptype_variant _ -> put_distrib ~params rec_flg loc tdecl
     | Ptype_record fields ->
       put_distrib ~params rec_flg loc tdecl
       @ [ mk_record_constr ~loc tdecl.ptype_name.txt fields ]
     | _ -> failwith "Only variant types without manifest are supported")
  | Old_OCanren | Only_injections | Distribs -> revisit_type ~params rec_flg loc tdecl
;;

let main_mapper params =
  let wrap_tydecls rec_flg loc ts =
    let f tdecl =
      if has_to_gen_attr tdecl.ptype_attributes
      then (
        let is_supported =
          match tdecl.ptype_kind with
          | Ptype_variant _ | Ptype_record _ -> tdecl.ptype_manifest = None
          | Ptype_abstract -> true
          | _ -> false
        in
        if is_supported
        then translate_type ~params rec_flg loc tdecl
        else failwith "Only variant types without manifest are supported")
      else [ str_type_ ~loc rec_flg [ tdecl ] ]
    in
    List.flatten (List.map f ts)
  in
  let rec mapper =
    { Ast_mapper.default_mapper with
      structure =
        (fun self ss ->
          let f si =
            match si.pstr_desc with
            | Pstr_type (rec_flg, tydecls) -> wrap_tydecls rec_flg si.pstr_loc tydecls
            | _ -> [ default_mapper.structure_item mapper si ]
          in
          List.concat_map f ss)
    }
  in
  mapper
;;

let process params =
  let mapper = main_mapper params in
  mapper.structure mapper
;;
