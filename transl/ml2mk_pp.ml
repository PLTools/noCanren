open Config
open Clflags
open Compenv


module Compile  = struct
(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 2002 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* The batch compiler *)

open Misc
open Format
open Typedtree
open Compenv

(* Compile a .mli file *)

(* Keep in sync with the copy in optcompile.ml *)

let tool_name = "ocamlc"

let interface ppf sourcefile outputprefix =
  failwith "interfaces are not supported"
  (*Compmisc.init_path false;
  let modulename = module_of_filename ppf sourcefile outputprefix in
  Env.set_unit_name modulename;
  let initial_env = Compmisc.initial_env () in
  let ast = Pparse.parse_interface ~tool_name ppf sourcefile in
  if !Clflags.dump_parsetree then fprintf ppf "%a@." Printast.interface ast;
  if !Clflags.dump_source then fprintf ppf "%a@." Pprintast.signature ast;
  let tsg = Typemod.type_interface initial_env ast in
  if !Clflags.dump_typedtree then fprintf ppf "%a@." Printtyped.interface tsg;
  let sg = tsg.sig_type in
  if !Clflags.print_types then
    Printtyp.wrap_printing_env initial_env (fun () ->
        fprintf std_formatter "%a@."
          Printtyp.signature (Typemod.simplify_signature sg));
  ignore (Includemod.signatures initial_env sg sg);
  Typecore.force_delayed_checks ();
  Warnings.check_fatal ();
  if not !Clflags.print_types then begin
    let sg = Env.save_signature sg modulename (outputprefix ^ ".cmi") in
    Typemod.save_signature modulename tsg outputprefix sourcefile
      initial_env sg ;
  end*)

(* Compile a .ml file *)

let print_if ppf flag printer arg =
  if !flag then fprintf ppf "%a@." printer arg;
  arg

let (++) x f = f x

let implementation ppf sourcefile outputprefix =
  Compmisc.init_path false;
  Format.set_margin 120;
  let modulename = module_of_filename ppf sourcefile outputprefix in
  Env.set_unit_name modulename;
  let env = Compmisc.initial_env() in
  let where_to_print = match !output_name with Some s -> s | None -> failwith "output filenot specified" in
  try
(*    Format.printf "output prefix = %s, output_name = %s\n%!" !output_name outputprefix;*)
    let (typedtree, coercion) =
      Pparse.parse_implementation ~tool_name ppf sourcefile
      ++ (fun p  -> print_endline "parsed"; p)
      ++ print_if ppf Clflags.dump_parsetree Printast.implementation
      ++ print_if ppf Clflags.dump_source Pprintast.structure
      ++ Typemod.type_implementation sourcefile outputprefix modulename env
      ++ print_if ppf Clflags.dump_typedtree
        Printtyped.implementation_with_coercion
    in
    let untyped = Genmain.only_generate { Misc.sourcefile = sourcefile } typedtree in
    print_endline "plugin finished";
    let () = Pprintast.structure Format.std_formatter untyped in

    let () =
      let ch = open_out where_to_print in
      let fmt = Format.formatter_of_out_channel ch in
      Pprintast.structure fmt untyped
    in
    ()
(*
    if !Clflags.print_types then begin
      Warnings.check_fatal ();
      Stypes.dump (Some (outputprefix ^ ".annot"))
    end else begin
      let bytecode =
        (typedtree, coercion)
        ++ Translmod.transl_implementation modulename
        ++ print_if ppf Clflags.dump_rawlambda Printlambda.lambda
        ++ Simplif.simplify_lambda
        ++ print_if ppf Clflags.dump_lambda Printlambda.lambda
        ++ Bytegen.compile_implementation modulename
        ++ print_if ppf Clflags.dump_instr Printinstr.instrlist
      in
      let objfile = outputprefix ^ ".cmo" in
      let oc = open_out_bin objfile in
      try
        bytecode
        ++ Emitcode.to_file oc modulename objfile;
        Warnings.check_fatal ();
        close_out oc;
        Stypes.dump (Some (outputprefix ^ ".annot"))
      with x ->
        close_out oc;
        remove_file objfile;
        raise x
    end *)
  with
    | Misc.HookExnWrapper wr -> raise wr.Misc.error
    | x -> raise x

let c_file name =
  Location.input_name := name;
  if Ccomp.compile_file name <> 0 then exit 2

end

let process_interface_file ppf name =
  Compile.interface ppf name (output_prefix name)

let process_implementation_file ppf name =
  let opref = output_prefix name in
  Compile.implementation ppf name opref;
  objfiles := (opref ^ ".cmo") :: !objfiles

let process_file ppf name =
  Printf.printf "process_file `%s`\n%!" name;
  if Filename.check_suffix name ".ml"
  || Filename.check_suffix name ".mlt" then begin
    let opref = output_prefix name in
    Compile.implementation ppf name opref;
    objfiles := (opref ^ ".cmo") :: !objfiles
  end (*
  else if Filename.check_suffix name !Config.interface_suffix then begin
    let opref = output_prefix name in
    Compile.interface ppf name opref;
    if !make_package then objfiles := (opref ^ ".cmi") :: !objfiles
  end
  else if Filename.check_suffix name ".cmo"
       || Filename.check_suffix name ".cma" then
    objfiles := name :: !objfiles
  else if Filename.check_suffix name ".cmi" && !make_package then
    objfiles := name :: !objfiles
  else if Filename.check_suffix name ext_obj
       || Filename.check_suffix name ext_lib then
    ccobjs := name :: !ccobjs
  else if Filename.check_suffix name ext_dll then
    dllibs := name :: !dllibs
  else if Filename.check_suffix name ".c" then begin
    Compile.c_file name;
    ccobjs := (Filename.chop_suffix (Filename.basename name) ".c" ^ ext_obj)
              :: !ccobjs
  end *)
  else
    raise(Arg.Bad("don't know what to do with " ^ name))

let usage = "Usage: ocamlc <options> <files>\nOptions are:"

let ppf = Format.err_formatter

(* Error messages to standard error formatter *)
let anonymous filename =
  readenv ppf Before_compile; process_file ppf filename;;
let impl filename =
  readenv ppf Before_compile; process_implementation_file ppf filename;;
let intf filename =
  readenv ppf Before_compile; process_interface_file ppf filename;;

let show_config () =
  Config.print_config stdout;
  exit 0;
;;


module Options = Main_args.Make_bytecomp_options (struct
  let set r () = r := true
  let unset r () = r := false
  let _a = set make_archive
  let _absname = set Location.absname
  let _annot = set annotations
  let _binannot = set binary_annotations
  let _c = set compile_only
  let _cc s = c_compiler := Some s
  let _cclib s = ccobjs := Misc.rev_split_words s @ !ccobjs
  let _ccopt s = first_ccopts := s :: !first_ccopts
  let _compat_32 = set bytecode_compatible_32
  let _config = show_config
  let _custom = set custom_runtime
  let _no_check_prims = set no_check_prims
  let _dllib s = dllibs := Misc.rev_split_words s @ !dllibs
  let _dllpath s = dllpaths := !dllpaths @ [s]
  let _for_pack s = for_package := Some s
  let _g = set debug
  let _i () = print_types := true; compile_only := true
  let _I s = include_dirs := s :: !include_dirs
  let _impl = impl
  let _intf = intf
  let _intf_suffix s = Config.interface_suffix := s
  let _keep_docs = set keep_docs
  let _keep_locs = set keep_locs
  let _labels = unset classic
  let _linkall = set link_everything
  let _make_runtime () =
    custom_runtime := true; make_runtime := true; link_everything := true
  let _no_alias_deps = set transparent_modules
  let _no_app_funct = unset applicative_functors
  let _noassert = set noassert
  let _nolabels = set classic
  let _noautolink = set no_auto_link
  let _nostdlib = set no_std_include
  let _o s = output_name := Some s
  let _open s = open_modules := s :: !open_modules
  let _output_obj () = output_c_object := true; custom_runtime := true
  let _output_complete_obj () =
    output_c_object := true; output_complete_object := true; custom_runtime := true
  let _pack = set make_package
  let _pp s = preprocessor := Some s
  let _ppx s = first_ppx := s :: !first_ppx
  let _plugin p = Compplugin.load p
  let _principal = set principal
  let _rectypes = set recursive_types
  let _runtime_variant s = runtime_variant := s
  let _safe_string = unset unsafe_string
  let _short_paths = unset real_paths
  let _strict_sequence = set strict_sequence
  let _strict_formats = set strict_formats
  let _thread = set use_threads
  let _vmthread = set use_vmthreads
  let _unsafe = set fast
  let _unsafe_string = set unsafe_string
  let _use_prims s = use_prims := s
  let _use_runtime s = use_runtime := s
  let _v () = print_version_and_library "compiler"
  let _version = print_version_string
  let _vnum = print_version_string
  let _w = (Warnings.parse_options false)
  let _warn_error = (Warnings.parse_options true)
  let _warn_help = Warnings.help_warnings
  let _where = print_standard_library
  let _verbose = set verbose
  let _nopervasives = set nopervasives
  let _dsource = set dump_source
  let _dparsetree = set dump_parsetree
  let _dtypedtree = set dump_typedtree
  let _drawlambda = set dump_rawlambda
  let _dlambda = set dump_lambda
  let _dinstr = set dump_instr
  let anonymous = anonymous
end)


let () =
    readenv ppf Before_args;
    Arg.parse Options.list anonymous usage;
    ()

