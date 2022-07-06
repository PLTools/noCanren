open Parsetree
open Util

let rec get_arg_types (t : Types.type_expr) =
  match Types.get_desc t with
  | Tarrow (_, a, b, _) -> a :: get_arg_types b
  | Tlink t' -> get_arg_types t'
  | _ -> []
;;

let rec compress fresher name typ =
  let loc = Ppxlib.Location.none in
  let arg_types = get_arg_types typ in
  let res_var = fresher () in
  let abstrs, exprs =
    List.fold_right
      (fun arg (abstrs, exprs) ->
        let var = fresher () in
        let abstrs r = create_fun var (abstrs r) in
        let new_arg =
          if is_func_type arg
          then uncompress fresher var arg
          else [%expr ( === ) [%e create_id var]]
        in
        abstrs, new_arg :: exprs)
      arg_types
      ((fun r -> create_fun res_var r), [ create_id res_var ])
  in
  abstrs (create_apply (create_id name) exprs)

and uncompress fresher name typ =
  let arg_types = get_arg_types typ in
  let res_var = fresher () in
  let abstrs, freshes, conjs, args =
    List.fold_right
      (fun arg (abstrs, freshes, conjs, args) ->
        let var = fresher () in
        let abstrs r = create_fun var (abstrs r) in
        if is_func_type arg
        then abstrs, freshes, conjs, compress fresher var arg :: args
        else (
          let fr_var = fresher () in
          let freshes r = create_fresh fr_var (freshes r) in
          let conjs = create_apply (create_id var) [ create_id fr_var ] :: conjs in
          let args = create_id fr_var :: args in
          abstrs, freshes, conjs, args))
      arg_types
      ((fun r -> create_fun res_var r), Fun.id, [], [ create_id res_var ])
  in
  abstrs (freshes (create_conj (conjs @ [ create_apply (create_id name) args ])))
;;
