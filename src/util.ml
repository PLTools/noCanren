open Longident

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

    output_name_for_spec_tree : string option;
  }

(************************** Util funcions *******************************)

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
