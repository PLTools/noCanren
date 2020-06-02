open Config
open Clflags
open Compenv
open Misc
open Format
open Typedtree
open Util

let need_print_result = ref false

let tool_name = "noCanren"

let print_if ppf flag printer arg =
  if !flag then fprintf ppf "%a@." printer arg;
  arg

let (++) x f = f x

let translate ppf params =
  Clflags.include_dirs := List.append params.include_dirs !Clflags.include_dirs;
  Clflags.open_modules := List.append params.opens !Clflags.open_modules;
  Compmisc.init_path false;
  let outputprefix = output_prefix params.input_name in
  let modulename = module_of_filename ppf params.input_name outputprefix in
  Env.set_unit_name modulename;
  let env = Compmisc.initial_env() in
  try
    let (typedtree, coercion) =
      Pparse.parse_implementation ~tool_name ppf params.input_name
      ++ print_if ppf Clflags.dump_parsetree Printast.implementation
      ++ print_if ppf Clflags.dump_source Pprintast.structure
      ++ Typemod.type_implementation params.input_name outputprefix modulename env
      ++ print_if ppf Clflags.dump_typedtree
        Printtyped.implementation_with_coercion
    in
    let untyped = Translator.only_generate { Misc.sourcefile = params.input_name } typedtree params in
    let tree_without_attrs = Translator.(attrs_remover.structure attrs_remover) untyped in
    let () = if !need_print_result || params.output_name == None
             then Pprintast.structure Format.std_formatter tree_without_attrs in

    let () =
      match params.output_name with
      | None             -> ()
      | Some output_name ->
        let ch = open_out output_name in
        let fmt = Format.formatter_of_out_channel ch in
        Format.pp_set_margin fmt 180;
        Pprintast.structure fmt tree_without_attrs;
        fprintf fmt "%!" in

    let () =
      match params.output_name_for_spec_tree with
      | None      -> ()
      | Some path ->
        let ch = open_out path in
        let fmt = Format.formatter_of_out_channel ch in
        Printer_in_spec.print_tree fmt untyped in
    ()

  with
    | Typetexp.Error (_loc,env,e) as exc ->
      Typetexp.report_error env Format.std_formatter e;
      Format.printf "\n%!";
      raise exc
    | x -> raise x


let usage = "Usage: noCanren <options> <file>\nOptions are:"

let ppf = Format.err_formatter

let input_name                  = ref None
let output_name                 = ref None
let include_dirs                = ref []
let opens                       = ref []
let unnesting_mode              = ref false
let activate_tactic             = ref Det
let use_call_by_need            = ref false
let polymorphism_supported      = ref false
let remove_false                = ref false
let use_standart_bool_relations = ref false
let beta_reduction              = ref true
let normalization               = ref true
let move_unifications           = ref true
let leave_constuctors           = ref false
let subst_only_util_vars        = ref false
let output_name_for_spec_tree   = ref None

let all_options =
  [
    "-o",
    Arg.String (fun path -> output_name := Some path),
    "<file>  Set output file name to <file>"
    ;
    "-I",
    Arg.String (fun dir -> include_dirs := dir :: !include_dirs),
    "<dir>  Add <dir> to the list of include directories"
    ;
    "-open",
    Arg.String (fun o -> opens := o :: !opens),
    "<module>  Opens the module <module> before typing"
    ;
    "-high-order-mode",
    Arg.Unit (fun _ -> unnesting_mode := false),
    " Switch to high-order mode"
    ;
    "-unnesting-mode",
    Arg.Unit (fun _ -> unnesting_mode := true),
    " Switch to unnesting mode"
    ;
    "-without-activate-tactics",
    Arg.Unit (fun _ -> activate_tactic := Off),
    " Disable activate tactic (only for high-order mode)"
    ;
    "-non-deterministic-activate-tactic",
    Arg.Unit (fun _ -> activate_tactic := Nondet),
    " Use non-deterministic activate tactic (only for high-order mode)"
    ;
    "-deterministic-activate-tactic",
    Arg.Unit (fun _ -> activate_tactic := Det),
    " Use deterministic activate tactic (only for high-order mode)"
    ;
    "-use-call-by-need",
    Arg.Unit (fun _ -> use_call_by_need := true),
    " Use call-by-need extension of high-order mode (only for high-order mode)"
    ;
    "-need-polymorphism",
    Arg.Unit (fun _ -> polymorphism_supported := true),
    " For supporting of polymorphism (only for unnesting mode)"
    ;
    "-without-false",
    Arg.Unit (fun _ -> remove_false := true),
    " For simplified logical relational (only for unnesting mode)"
    ;
    "-standart-bool",
    Arg.Unit (fun _ -> use_standart_bool_relations := true),
    " Enable standart bool relations (only for unnesting mode)"
    ;
    "-without-beta-reduction",
    Arg.Unit (fun _ -> beta_reduction := false),
    " Disable beta-redactions after conversion (for both modes)"
    ;
    "-without-normalization",
    Arg.Unit (fun _ -> normalization := false),
    " Disable normalization after conversion (for both modes)"
    ;
    "-not-move-unifications",
    Arg.Unit (fun _ -> move_unifications := false),
    " Don't move unifications and disequality constrains after conversion (for both modes)"
    ;
    "-leave-constuctors",
    Arg.Unit (fun _ -> leave_constuctors := true),
    " Conversion is without lawercase-renaming of constructors (for both modes)"
    ;
    "-subst-only-util-vars",
    Arg.Unit (fun _ -> subst_only_util_vars := true),
    " Use beta-reduction only for additional variables (for both modes)"
    ;
    "-spec-tree",
    Arg.String (fun path -> output_name_for_spec_tree := Some path),
    "<file>  Set output file name for specialization tree to <file>"
    ;
    "-show-result",
    Arg.Unit (fun path -> need_print_result := true),
    " Show result of conversion in terminal"
  ]

let mk_noCanren_params () =
  if !unnesting_mode then begin
    if !activate_tactic <> Off then failwith "Don't use activate tactic with unnesting mode."
    else if !use_call_by_need then failwith "Don't use call-by-need qith unnesting mode."
    end
  else begin
    if !remove_false then failwith "False cannot be deleted with high-order mode."
    else if !use_standart_bool_relations then failwith "Standart bool relations cannot be used with high-order mode."
    end;
  let input_name = match !input_name with Some s -> s | None -> failwith "Input file not specified" in
  let output_name = !output_name in
  let unnesting_params =
  {
    polymorphism_supported = !polymorphism_supported;
    remove_false = !remove_false;
    use_standart_bool_relations = !use_standart_bool_relations
  } in
  let high_order_paprams =
  {
    activate_tactic = !activate_tactic;
    use_call_by_need = !use_call_by_need
  } in
  {
    input_name = input_name;
    output_name = output_name;
    include_dirs = !include_dirs;
    opens = !opens;
    unnesting_mode = !unnesting_mode;
    beta_reduction = !beta_reduction;
    normalization = !normalization;
    move_unifications = !move_unifications;
    leave_constuctors = !leave_constuctors;
    subst_only_util_vars = !subst_only_util_vars;
    high_order_paprams = high_order_paprams;
    unnesting_params = unnesting_params;

    output_name_for_spec_tree = !output_name_for_spec_tree;
  }

let () =
    Arg.parse all_options (fun path -> input_name := Some path) usage;
    try begin
      readenv ppf Before_args;
      translate ppf @@ mk_noCanren_params ()
    end with Failure     s -> (Printf.printf "%s\n%!" s; Arg.usage all_options usage)
           | Sys_error   s -> Printf.printf "%s\n%!" s
           | e             -> begin match Location.error_of_exn e with
                              | Some (`Ok e) -> Format.eprintf "%a\n%!" Location.report_error e
                              | _            -> raise e
                              end
