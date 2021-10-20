open Asttypes
open Longident
open Typedtree
open Ast_helper
open Ident
open Parsetree

(**************************** Util types ********************************)

type tactic = Off | Nondet | Det

type noCanren_high_params =
  {
    activate_tactic  : tactic;
    use_call_by_need : bool
  }

type noCanren_unnesting_params =
  {
    polymorphism_supported      : bool;
    remove_false                : bool;
    use_standart_bool_relations : bool
  }

type noCanren_params =
  {
    input_name                : string;
    output_name               : string option;
    include_dirs              : string list;
    opens                     : string list;
    unnesting_mode            : bool;
    beta_reduction            : bool;
    normalization             : bool;
    move_unifications         : bool;
    leave_constuctors         : bool;
    subst_only_util_vars      : bool;
    high_order_paprams        : noCanren_high_params;
    unnesting_params          : noCanren_unnesting_params;
    useGT                    : bool;

    output_name_for_spec_tree : string option;
  }

(***************************** Constants **********************************)

let fresh_var_prefix    = "q"
let tabling_attr_name   = "tabled"

let fresh_module_name   = "Fresh"
let fresh_one_name      = "one"
let fresh_two_name      = "two"
let fresh_three_name    = "three"
let fresh_four_name     = "four"
let fresh_five_name     = "five"
let fresh_succ_name     = "succ"


let packages = ["GT"; "OCanren"; "OCanren.Std"]

(***************************** Fail util **********************************)

type error = NotYetSupported of string
exception TranslatorError of error


let report_error fmt  = function
| NotYetSupported s -> Format.fprintf fmt "Not supported during relational conversion: %s\n%!" s


let fail_loc loc fmt =
  let b = Buffer.create 100 in
  let f = Format.formatter_of_buffer b in
  let () = Format.fprintf f fmt in
  let () = Format.fprintf f ". " in
  let () = Location.print_loc f loc in
  Format.pp_print_flush f ();
  failwith (Buffer.contents b)

(************************** Util funcions *******************************)

let get_max_index tast =

  let max_index = ref 0 in

  let set_max_index_from_name name =
    let prefix_length = String.length fresh_var_prefix in
    let length = String.length name in
    if length > prefix_length && (String.sub name 0 prefix_length) = fresh_var_prefix then
      let index = try String.sub name prefix_length (length - prefix_length) |> int_of_string with Failure _ -> -1
      in if index > !max_index then max_index := index in

  let expr sub x =
    match x.exp_desc with
    | Texp_ident (path, _, _) -> Path.name path |> set_max_index_from_name; x
    | _                       -> Tast_mapper.default.expr sub x in

  let finder = {Tast_mapper.default with expr} in

  finder.structure finder tast |> ignore; !max_index


let mangle_construct_name name =
  let low = String.mapi (function 0 -> Char.lowercase_ascii | _ -> fun x -> x ) name in
  match low with
  | "val" | "if" | "else" | "for" | "do" | "let" | "open" | "not" | "pair" | "conj"
          | "var" | "snd" | "fst" -> low ^ "_"
  | _ -> low


let rec longident_eq a b =
  match a, b with
  | Lident x,        Lident y        -> x = y
  | Ldot (a, x),     Ldot (b, y)     -> x = y && longident_eq a b
  | Lapply (a1, a2), Lapply (b1, b2) -> longident_eq a1 b1 && longident_eq a2 b2
  | _                                -> false


let rec lowercase_lident = function
  | Lident s      -> Lident (mangle_construct_name s)
  | Lapply (l, r) -> Lapply (lowercase_lident l, lowercase_lident r)
  | Ldot (t, s)   -> Ldot (t, mangle_construct_name s)

let rec fold_right1 f = function
| [h]  -> h
| h::t -> f h (fold_right1 f t)
| []   -> failwith "fold_right1: empty list"


let filteri f l =
  let rec filteri l n =
    match l with
    | []               -> []
    | x::xs when f n x -> x :: filteri xs (n+1)
    | _::xs            -> filteri xs (n+1) in
  filteri l 0


let rec split3 = function
| []         -> [], [], []
| (h1,h2,h3)::t -> let t1, t2, t3 = split3 t in h1::t1, h2::t2, h3::t3

(********************** Translator util funcions ****************************)

let untyper = Untypeast.default_mapper


let create_id  s = Lident s |> mknoloc |> Exp.ident


let create_pat s = mknoloc s |> Pat.var


let rec is_primary_type (t : Types.type_expr) =
  match t.desc with
  | Tarrow _ -> false
  | Tlink t' -> is_primary_type t'
  | _        -> true


let pat_is_var p =
  match p.pat_desc with
  | Tpat_var (_, _) -> true
  | _               -> false


let get_pat_name p =
  match p.pat_desc with
  | Tpat_var (n, _) -> name n
  | _               -> fail_loc p.pat_loc "Incorrect pattern"


let create_apply f = function
| []   -> f
| args ->
  let args = List.map (fun a -> Nolabel, a) args in
  match f.pexp_desc with
  | Pexp_apply (g, args') -> Exp.apply g (args' @ args)
  | _                     -> Exp.apply f args


let create_apply_to_list f arg_list =
  let loc = f.pexp_loc in
  let new_arg = List.fold_right (fun x acc -> [%expr [%e x] :: [%e acc]]) arg_list [%expr []] in
  create_apply f [new_arg]


let create_conj = function
| []     -> failwith "Conjunction needs one or more arguments"
| [x]    -> x
| [x; y] -> let loc = Ppxlib.Location.none in [%expr [%e x] &&& [%e y]]
| l      -> let loc = Ppxlib.Location.none in create_apply_to_list [%expr (?&)] l


let create_disj = function
| []     -> failwith "Conjunction needs one or more arguments"
| [x]    -> x
| [x; y] -> let loc = Ppxlib.Location.none in [%expr [%e x] ||| [%e y]]
| l      -> let loc = Ppxlib.Location.none in create_apply_to_list [%expr conde] l


let create_fun var body =
  let loc = Ppxlib.Location.none in
  [%expr fun [%p create_pat var] -> [%e body]]


let create_fresh var body =
  let loc = Ppxlib.Location.none in
  create_apply [%expr call_fresh] [create_fun var body]


let create_inj expr =
  let loc = Ppxlib.Location.none in
  [%expr !! [%e expr]]


let rec path2ident = function
  | Path.Pident i      -> Lident (name i)
  | Path.Pdot (l, r)   -> Ldot (path2ident l, r)
  | Path.Papply (l, r) -> Lapply (path2ident l, path2ident r)


let ctor_for_record loc typ =
  let rec get_id (typ : Types.type_expr) =
    match typ.desc with
    | Tlink t           -> get_id t
    | Tconstr (p, _, _) -> begin match path2ident p with
                           | Lident i    -> Lident ("ctor_g" ^ i)
                           | Ldot (l, r) -> Ldot (l, "ctor_g" ^ r)
                           | Lapply _    -> fail_loc loc "What is 'Lapply'?"
                           end
    | _                 -> fail_loc loc "Incorrect type of record" in
  get_id typ |> mknoloc |> Exp.ident


let filter_vars vars1 vars2 =
  List.filter (fun v -> List.for_all ((<>) v) vars2) vars1


let mark_type_declaration td =
    match td.typ_kind with
    | Ttype_variant _
    | Ttype_record  _ -> { td with typ_attributes = [Attr.mk (mknoloc "put_distrib_here") (Parsetree.PStr [])] }
    | _               -> fail_loc td.typ_loc "Incrorrect type declaration"


let mark_constr expr = { expr with
  pexp_attributes = (Attr.mk (mknoloc "it_was_constr") (Parsetree.PStr [])) :: expr.pexp_attributes }


let mark_fo_arg expr = { expr with
  pexp_attributes = (Attr.mk (mknoloc "need_CbN") (Parsetree.PStr [])) :: expr.pexp_attributes }


let is_active_arg pat =
  List.exists (fun a -> a.attr_name.txt = "active") pat.pat_attributes


let create_logic_var name =
  { (create_pat name) with ppat_attributes = [Attr.mk (mknoloc "logic") (Parsetree.PStr [])] }


let rec have_unifier p1 p2 =
  match p1.pat_desc, p2.pat_desc with
  | Tpat_any  , _ | _, Tpat_any
  | Tpat_var _, _ | _, Tpat_var _ -> true
  | Tpat_constant c1, Tpat_constant c2 -> c1 = c2
  | Tpat_tuple t1, Tpat_tuple t2 ->
    List.length t1 = List.length t2 && List.for_all2 have_unifier t1 t2
  | Tpat_construct (_, cd1, a1), Tpat_construct (_, cd2, a2) ->
    cd1.cstr_name = cd2.cstr_name && List.length a1 = List.length a2 && List.for_all2 have_unifier a1 a2
  | _ -> false


let rec translate_pat pat fresher =
  let loc = pat.pat_loc in
  match pat.pat_desc with
  | Tpat_any                                       -> let var = fresher () in create_id var, [], [var]
  | Tpat_var (v, _)                                -> create_id (name v), [], [name v]
  | Tpat_constant c                                -> Untypeast.constant c |> Exp.constant |> create_inj, [], []
  | Tpat_construct ({txt = Lident "true"},  _, []) -> [%expr !!true],  [], []
  | Tpat_construct ({txt = Lident "false"}, _, []) -> [%expr !!false], [], []
  | Tpat_construct ({txt = Lident "[]"},    _, []) -> [%expr [%e mark_constr [%expr nil]] ()], [], []
  | Tpat_construct (id              ,       _, []) -> [%expr [%e lowercase_lident id.txt |> mknoloc |> Exp.ident |> mark_constr] ()], [], []
  | Tpat_construct ({txt}, _, args)                ->
    let args, als, vars = List.map (fun q -> translate_pat q fresher) args |> split3 in
    let vars = List.concat vars in
    let als  = List.concat als  in
    let constr =
      match txt with
      | Lident "::" -> [%expr (%)]
      | _           -> [%expr [%e lowercase_lident txt |> mknoloc |> Exp.ident]] in
    create_apply (mark_constr constr) args, als, vars
  | Tpat_tuple l ->
    let args, als, vars = List.map (fun q -> translate_pat q fresher) l |> split3 in
    let vars = List.concat vars in
    let als  = List.concat als in
    fold_right1 (fun e1 e2 -> create_apply (mark_constr [%expr pair]) [e1; e2]) args, als, vars
  | Tpat_record (fields, _) ->
    let (_, info, _) = List.hd fields in
    let count = Array.length info.lbl_all in
    let rec translate_record_pat fresher fields index =
      if index == count then [], [], [] else
      match fields with
      | (_, (i : Types.label_description), _) :: _xs when i.lbl_pos > index ->
        let var        = fresher () in
        let pats, als, vars = translate_record_pat fresher fields (index+1) in
        create_id var :: pats, als, var :: vars
      | (_, _i, p) :: xs ->
        let pat , als,  vars  = translate_pat p fresher in
        let pats, als', vars' = translate_record_pat fresher xs (index+1) in
        pat :: pats, als @ als', vars @ vars'
      | [] ->
        let var       = fresher () in
        let pats, als, vars = translate_record_pat fresher [] (index+1) in
        create_id var :: pats, als, var :: vars in
    let args, als, vars = translate_record_pat fresher fields 0 in
    let ctor            = ctor_for_record pat.pat_loc pat.pat_type in
    create_apply ctor args, als, vars
  | Tpat_alias (p, v, _) ->
    let pat, als, vars = translate_pat p fresher in
    create_id (name v), (create_id (name v), pat) :: als, name v :: vars
  | _ -> fail_loc pat.pat_loc "Incorrect pattern in pattern matching"


let rec is_disj_pats = function
  | []      -> true
  | x :: xs -> not (List.exists (have_unifier x) xs) && is_disj_pats xs

let id2id_o = function
  | Lident s    -> Lident (s ^ "_o")
  | Ldot (t, s) -> Ldot (t, s ^ "_o")
  | _           -> failwith "id2id_o: undexpected ID"
