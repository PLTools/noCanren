open Ast_mapper
open Parsetree

module TypeNameMap = Map.Make(String)

module FoldInfo = struct
  type item = {param_name:string; rtyp: core_type; ltyp: core_type}
  exception ItemFound of item
  type t = item list

  let param_for_rtyp typ ts =
    let typ_repr =
      Printast.core_type 0 Format.str_formatter typ;
      Format.flush_str_formatter ()
    in
    try List.iter (fun i ->
                    let new_repr =
                      Printast.core_type 0 Format.str_formatter i.rtyp;
                      Format.flush_str_formatter ()
                    in
                    if new_repr = typ_repr then raise (ItemFound i)
                  ) ts;
        None
    with ItemFound i -> Some i

    let empty = []
    let extend param_name rtyp ltyp ts =
      {param_name; rtyp; ltyp} :: ts
end

let revisit_adt tdecl ctors =
  let der_typ_name = tdecl.ptype_name.Asttypes.txt in
  (* Let's forget about mutal recursion for now *)
  (* For every constructor argument we need to put ground types to parameters *)
  List.fold_right
    (fun cd (acc_map,cs) ->
        let map2,new_args = List.fold_right
          (fun typ (map,args) ->
                match typ with
                | [%type: _] -> assert false
                | {ptyp_desc = Ptyp_constr ({txt=Lident name;_},[]); _} when name = der_typ_name ->
                    (FoldInfo.extend "self" typ typ map, [%type: 'self] :: args)
                | arg -> (map, arg::args)
          )
          cd.pcd_args
          (acc_map,[])
        in
        (map2, { cd with pcd_args = new_args } :: cs)
    )
    ctors
    (FoldInfo.empty, [])


let main_mapper =
  let wrap_tydecls ts =
    let f tdecl =
      match tdecl.ptype_kind with
      | Ptype_variant cs when tdecl.ptype_manifest = None ->
          [Pstr_type [tdecl]]
      | _ -> failwith "Only variant types without manifest are supported"
    in
    List.flatten (List.map f ts)
  in

  { Ast_mapper.default_mapper with
    structure = fun self ss ->
      let f si = match si.pstr_desc with
      | Pstr_type tydecls -> List.map (fun pstr_desc -> {si with pstr_desc}) (wrap_tydecls tydecls)
      | x -> [si]
      in
      List.flatten (List.map f ss)
  }

let process x =
(*  main_mapper.structure main_mapper x*)
  x
