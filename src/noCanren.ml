open Compenv
open Format
open Util

let need_print_result = ref false
let tool_name = "noCanren"

let print_if ppf flag printer arg =
  if !flag then fprintf ppf "%a@." printer arg;
  arg
;;

let get_translator tast params =
  if params.unnesting_mode
  then Translator_unnesting.only_generate tast params
  else Translator.only_generate tast params
;;

let translate ppf params =
  Clflags.include_dirs
    := if params.need_std
       then List.append (get_std_lib_pathes ()) !Clflags.include_dirs
       else !Clflags.include_dirs;
  Clflags.include_dirs := List.append params.include_dirs !Clflags.include_dirs;
  Clflags.open_modules := List.append params.opens !Clflags.open_modules;
  Compmisc.init_path ();
  let outputprefix = output_prefix params.input_name in
  let modulename = module_of_filename params.input_name outputprefix in
  Env.set_unit_name modulename;
  let env = Compmisc.initial_env () in
  try
    let { Typedtree.structure = typedtree; _ } =
      Pparse.parse_implementation ~tool_name params.input_name
      |> print_if ppf Clflags.dump_parsetree Printast.implementation
      |> print_if ppf Clflags.dump_source Pprintast.structure
      |> Typemod.type_implementation params.input_name outputprefix modulename env
      |> print_if ppf Clflags.dump_typedtree Printtyped.implementation_with_coercion
    in
    let untyped = get_translator typedtree params in
    let tree_without_attrs = Util.(attrs_remover.structure attrs_remover) untyped in
    let () =
      if !need_print_result || params.output_name == None
      then Pprintast.structure Format.std_formatter tree_without_attrs
    in
    let () =
      match params.output_name with
      | None -> ()
      | Some output_name ->
        let ch = open_out output_name in
        let fmt = Format.formatter_of_out_channel ch in
        Format.pp_set_margin fmt 180;
        Pprintast.structure fmt tree_without_attrs;
        fprintf fmt "%!"
    in
    let () =
      match params.output_name_for_spec_tree with
      | None -> ()
      | Some path ->
        let ch = open_out path in
        let fmt = Format.formatter_of_out_channel ch in
        Printer_in_spec.print_tree fmt untyped
    in
    ()
  with
  | Typetexp.Error (_loc, env, e) as exc ->
    Typetexp.report_error env Format.std_formatter e;
    Format.printf "\n%!";
    raise exc
  | Util.TranslatorError e ->
    Format.eprintf "%a\n%!" Util.report_error e;
    exit 1
  | x -> raise x
;;

let usage = "Usage: noCanren <options> <file>\nOptions are:"
let ppf = Format.err_formatter
let input_name = ref None
let output_name = ref None
let include_dirs = ref []
let opens = ref []
let unnesting_mode = ref false
let activate_tactic = ref Det
let use_call_by_need = ref false
let polymorphism_supported = ref false
let remove_false = ref false
let use_standart_bool_relations = ref false
let beta_reduction = ref true
let normalization = ref true
let move_unifications = ref true
let leave_constuctors = ref false
let subst_only_util_vars = ref false
let output_name_for_spec_tree = ref None
let useGT = ref false
let gen_info = ref Util.Only_distribs
let reexport_path = ref None
let syntax_extenstions = ref true
let need_std = ref true
let use_wildcard = ref true

module OcamlcOptions = Main_args.Make_bytecomp_options (Main_args.Default.Main)

let all_options =
  [ ( "-o"
    , Arg.String (fun path -> output_name := Some path)
    , "<file>  Set output file name to <file>" )
  ; ( "-I"
    , Arg.String (fun dir -> include_dirs := dir :: !include_dirs)
    , "<dir>  Add <dir> to the list of include directories" )
  ; ( "-open"
    , Arg.String (fun o -> opens := o :: !opens)
    , "<module>  Opens the module <module> before typing" )
  ; ( "-high-order-mode"
    , Arg.Unit (fun _ -> unnesting_mode := false)
    , " Switch to high-order mode" )
  ; ( "-unnesting-mode"
    , Arg.Unit (fun _ -> unnesting_mode := true)
    , " Switch to unnesting mode" )
  ; ( "-without-activate-tactics"
    , Arg.Unit (fun _ -> activate_tactic := Off)
    , " Disable activate tactic (only for high-order mode)" )
  ; ( "-non-deterministic-activate-tactic"
    , Arg.Unit (fun _ -> activate_tactic := Nondet)
    , " Use non-deterministic activate tactic (only for high-order mode)" )
  ; ( "-deterministic-activate-tactic"
    , Arg.Unit (fun _ -> activate_tactic := Det)
    , " Use deterministic activate tactic (only for high-order mode)" )
  ; ( "-use-call-by-need"
    , Arg.Unit (fun _ -> use_call_by_need := true)
    , " Use call-by-need extension of high-order mode (only for high-order mode)" )
  ; ( "-need-polymorphism"
    , Arg.Unit (fun _ -> polymorphism_supported := true)
    , " For supporting of polymorphism (only for unnesting mode)" )
  ; ( "-without-false"
    , Arg.Unit (fun _ -> remove_false := true)
    , " For simplified logical relational (only for unnesting mode)" )
  ; ( "-standart-bool"
    , Arg.Unit (fun _ -> use_standart_bool_relations := true)
    , " Enable standart bool relations (only for unnesting mode)" )
  ; ( "-without-beta-reduction"
    , Arg.Unit (fun _ -> beta_reduction := false)
    , " Disable beta-redactions after conversion (for both modes)" )
  ; ( "-without-normalization"
    , Arg.Unit (fun _ -> normalization := false)
    , " Disable normalization after conversion (for both modes)" )
  ; ( "-not-move-unifications"
    , Arg.Unit (fun _ -> move_unifications := false)
    , " Don't move unifications and disequality constrains after conversion (for both \
       modes)" )
  ; ( "-leave-constuctors"
    , Arg.Unit (fun _ -> leave_constuctors := true)
    , " Conversion is without lawercase-renaming of constructors (for both modes)" )
  ; ( "-subst-only-util-vars"
    , Arg.Unit (fun _ -> subst_only_util_vars := true)
    , " Use beta-reduction only for additional variables (for both modes)" )
  ; ( "-spec-tree"
    , Arg.String (fun path -> output_name_for_spec_tree := Some path)
    , "<file>  Set output file name for specialization tree to <file>" )
  ; ( "-show-result"
    , Arg.Unit (fun path -> need_print_result := true)
    , " Show result of conversion in terminal" )
  ; "-useGT", Arg.Unit (fun _ -> useGT := true), " Use GT in translated code"
  ; ( "-old-ocanren"
    , Arg.Unit (fun _ -> gen_info := Util.Old_OCanren)
    , " Generate interface for old oCanren (<0.3): FMap1/2/3, etc." )
  ; ( "-new-ocanren"
    , Arg.Unit (fun _ -> gen_info := Util.Only_injections)
    , " Generate just distribs for new OCanren" )
  ; ( "-distribs"
    , Arg.Unit (fun _ -> gen_info := Util.Distribs)
    , " Generate just distribs for new OCanren" )
  ; ( "-only-distribs"
    , Arg.Unit (fun _ -> gen_info := Util.Only_distribs)
    , " Generate only distribs for new OCanren" )
  ; "-dtypedtree", Arg.Unit (fun _ -> Clflags.dump_typedtree := true), " Trace typed tree"
  ; ( "-remove-syntax-extensions"
    , Arg.Unit (fun _ -> syntax_extenstions := false)
    , " Remove suntax extensions ('call_fresh' instead of 'fresh')" )
  ; ( "-reexport-path"
    , Arg.String
        (fun s ->
          reexport_path := if s = "" then Some [] else Some (String.split_on_char '.' s))
    , " Add a module path for reexporting of types" )
  ; "-std", Arg.Unit (fun _ -> need_std := true), "Use std libraries (default)"
  ; "-no-std", Arg.Unit (fun _ -> need_std := false), "Do not use std libraries."
  ; ( "-without-wildcards"
    , Arg.Unit (fun _ -> use_wildcard := false)
    , " Don't use relational wirldcards in output program." )
  ]
  @ OcamlcOptions.list
;;

(* TODO: Kakadu. Why we have multiple refs and we collect them into a record instead of a record with mutable fields??? *)

let mk_noCanren_params () =
  if !unnesting_mode
  then (
    if !activate_tactic <> Off
    then failwith "Don't use activate tactic with unnesting mode."
    else if !use_call_by_need
    then failwith "Don't use call-by-need qith unnesting mode.")
  else if !remove_false
  then failwith "False cannot be deleted with high-order mode."
  else if !use_standart_bool_relations
  then failwith "Standart bool relations cannot be used with high-order mode.";
  let input_name =
    match !input_name with
    | Some s -> s
    | None -> failwith "Input file not specified"
  in
  let output_name = !output_name in
  let unnesting_params =
    { polymorphism_supported = !polymorphism_supported
    ; remove_false = !remove_false
    ; use_standart_bool_relations = !use_standart_bool_relations
    }
  in
  let high_order_paprams =
    { activate_tactic = !activate_tactic; use_call_by_need = !use_call_by_need }
  in
  { input_name
  ; output_name
  ; include_dirs = !include_dirs
  ; opens = !opens
  ; unnesting_mode = !unnesting_mode
  ; beta_reduction = !beta_reduction
  ; normalization = !normalization
  ; move_unifications = !move_unifications
  ; leave_constuctors = !leave_constuctors
  ; subst_only_util_vars = !subst_only_util_vars
  ; high_order_paprams
  ; unnesting_params
  ; useGT = !useGT
  ; gen_info = !gen_info
  ; syntax_extenstions = !syntax_extenstions
  ; output_name_for_spec_tree = !output_name_for_spec_tree
  ; reexport_path = !reexport_path
  ; need_std = !need_std
  ; use_wildcard = !use_wildcard
  }
;;

let () =
  Arg.parse all_options (fun path -> input_name := Some path) usage;
  try
    readenv ppf Before_args;
    translate ppf @@ mk_noCanren_params ()
  with
  | Failure s ->
    Printf.eprintf "%s\n%!" s;
    exit 2
  | Sys_error s ->
    Printf.eprintf "%s\n%!" s;
    exit 2
  | e ->
    (match Location.error_of_exn e with
     | Some (`Ok e) ->
       Location.print_report ppf e;
       exit 2
     | _ -> raise_notrace e)
;;
