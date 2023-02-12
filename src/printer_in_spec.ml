open Parsetree
open Format

let print_tree
  ?(module_name = "program")
  ?(tree_name = "tree")
  ?(last_goal = "last_goal")
  fmt
  tree
  =
  let name_from_pat pat =
    match pat.ppat_desc with
    | Ppat_var loc -> loc.txt
    | _ -> failwith "Incorrect pattern in tree printer"
  in
  let rec has_attr attr = function
    | x :: xs -> x.attr_name.Location.txt = attr || has_attr attr xs
    | [] -> false
  in
  let rec get_args_and_body expr =
    match expr.pexp_desc with
    | Pexp_fun (_, _, pat, body) ->
      let vars, body' = get_args_and_body body in
      name_from_pat pat :: vars, body'
    | _ -> [], expr
  in
  let rec print_list need_nl sep f fmt = function
    | [ x ] -> f fmt x
    | x :: xs when need_nl ->
      fprintf fmt "%a%s@\n%a" f x sep (print_list need_nl sep f) xs
    | x :: xs -> fprintf fmt "%a%s%a" f x sep (print_list need_nl sep f) xs
    | [] -> ()
  in
  let expr_as_ident expr =
    match expr.pexp_desc with
    | Pexp_ident { txt = Lident name } -> name
    | _ -> failwith "Expression isn't ident"
  in
  let rec print_fresh fmt args =
    let vars, conjs =
      match args with
      | { pexp_desc = Pexp_apply (f, a) } :: xs -> f :: List.map snd a, xs
      | { pexp_desc = Pexp_tuple [ a ] } :: xs -> [ a ], xs
      | [] -> failwith "Fresh must have arguments"
      | _ -> failwith "Incorrect tree for fresh-operation"
    in
    fprintf
      fmt
      "@[<h2>fresh [%a] (@\n%a@])"
      (print_list false ", " (fun fmt e -> expr_as_ident e |> fprintf fmt "\"%s\""))
      vars
      (print_list true " &&& " print_expr_in_brackets)
      conjs
  and print_apply fmt expr =
    let rec normalize_apply expr =
      match expr.pexp_desc with
      | Pexp_apply (f, args) ->
        let f', args' = normalize_apply f in
        f', args' @ List.map snd args
      | _ -> expr, []
    in
    let rec get_args_from_list args =
      match args.pexp_desc with
      | Pexp_construct ({ txt = Lident "[]" }, _) -> []
      | Pexp_construct ({ txt = Lident "::" }, Some { pexp_desc = Pexp_tuple [ hd; tl ] })
        -> hd :: get_args_from_list tl
      | _ -> failwith "Bad args for fresh-operation in tree printer"
    in
    let f, args = normalize_apply expr in
    let name = expr_as_ident f in
    if has_attr "it_was_constr" f.pexp_attributes
    then (
      match args with
      | [ { pexp_desc = Pexp_construct ({ txt = Lident "()" }, None) } ] ->
        fprintf fmt "C \"%s\" []" name
      | _ -> fprintf fmt "C \"%s\" [%a]" name (print_list false ", " print_expr) args)
    else (
      match name, args with
      | "!!", [ a ] -> print_expr fmt a
      | "fresh", _ -> print_fresh fmt args
      | name, [ a1; a2 ] when name = "&&&" || name = "|||" ->
        fprintf fmt "(%a) %s @\n(%a)" print_expr a1 name print_expr a2
      | "===", [ a1; a2 ] -> fprintf fmt "%a === %a" print_expr a1 print_expr a2
      | "conde", [ a ] ->
        get_args_from_list a |> print_list true " ||| " print_expr_in_brackets fmt
      | "?&", [ a ] ->
        get_args_from_list a |> print_list true " &&& " print_expr_in_brackets fmt
      (*  | "=/=", _          -> failwith "Disequality constaint isn't supported in specializator" *)
      | _ -> fprintf fmt "call \"%s\" [%a]" name (print_list false ", " print_expr) args)
  and print_expr_in_brackets fmt = fprintf fmt "(%a)" print_expr
  and print_expr fmt expr =
    match expr.pexp_desc with
    | Pexp_construct ({ txt = Lident "true" }, None)
    | Pexp_construct ({ txt = Lident "false" }, None)
    | Pexp_constant _ -> fprintf fmt "C \"%a\" []" Pprintast.expression expr
    | Pexp_ident _ -> fprintf fmt "V \"%a\"" Pprintast.expression expr
    | Pexp_apply _ -> print_apply fmt expr
    | Pexp_let (_, vbs, expr) -> print_vbs (fun fmt () -> print_expr fmt expr) fmt vbs
    | _ -> failwith "Incorrect exprission in tree printing"
  and print_vb fmt vb =
    let vars, body = get_args_and_body vb.pvb_expr in
    fprintf
      fmt
      "@[<h2>Let (def \"%s\" [%a] (@\n%a@]@\n))"
      (name_from_pat vb.pvb_pat)
      (print_list false ", " (fun fmt -> fprintf fmt "\"%s\""))
      vars
      print_expr
      body
  and print_vbs cont fmt = function
    | vb :: vbs -> fprintf fmt "%a (@\n%a)" print_vb vb (print_vbs cont) vbs
    | [] -> cont fmt ()
  and print_structure_items fmt = function
    | { pstr_desc = Pstr_value (_, vbs) } :: xs ->
      print_vbs (fun fmt _ -> print_structure_items fmt xs) fmt vbs
    | [] -> fprintf fmt "%s" last_goal
    | _ -> failwith "Error in print_structure_items"
  in
  let tree_without_types, types =
    let one_step si (tr, ty) =
      match si.pstr_desc with
      | Pstr_value (_, vbs)
        when has_attr "service_function" (List.hd vbs).pvb_attributes |> not ->
        si :: tr, ty
      | _ -> tr, si :: ty
    in
    List.fold_right one_step tree ([], [])
  in
  let types = Util.(attrs_remover.structure attrs_remover) types in
  let buf = Buffer.create 100 in
  let buf_fmt = Format.formatter_of_buffer buf in
  Pprintast.structure buf_fmt types;
  Format.pp_print_flush buf_fmt ();
  let text = Buffer.contents buf |> String.escaped in
  fprintf fmt "@[module %s where @\n@\n@]" (String.capitalize_ascii module_name);
  fprintf fmt "@[import Syntax@\n@\n@]";
  fprintf
    fmt
    "@[<h2>%s = \n (\\%s -> @\n%a@\n@\n,@\n@\n\"%s\")%!@]"
    tree_name
    last_goal
    print_structure_items
    tree_without_types
    text
;;
