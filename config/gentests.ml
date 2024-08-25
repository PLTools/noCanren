let header =
  {|; THIS FILE IS GENERATED AUTOMATICALLY

(env
 (_
  (flags
   (:standard -rectypes -w -27-33-39))))|}
;;

let std_names = [ "peano"; "list"; "maybe" ]

let std_modules =
  let open Stdlib in
  String.concat " " @@ List.map String.capitalize_ascii std_names
;;

let std_deps =
  let open Stdlib in
  let mk name =
    Printf.sprintf
      "%%{project_root}/std/.%s.objs/byte/%s.cmi"
      (String.capitalize_ascii name)
      name
  in
  String.concat "\n  " @@ List.map mk std_names
;;

let wrap =
  let where =
    {|
(executable
 (name TEMPLATE_run)
 ;(package noCanren-tests)
 ;(public_name noCanren-tests.TEMPLATE)
 (libraries GT OCanren OCanren.tester MODULES)
 (modules TEMPLATE TEMPLATE_run)
 (preprocess
  (pps
   OCanren-ppx.ppx_repr
   OCanren-ppx.ppx_fresh
   OCanren-ppx.ppx_distrib
   -new-typenames
   GT.ppx_all)))

(rule
 (targets TEMPLATE.ml)
 (deps
  (:exec %{project_root}/src/noCanren.exe)
  DEPS
  (:input TEMPLATE.ml2mk.ml))
 (action
  (run
   sh
   -c
   "%{exec} -w -8 %{input} -rectypes -o %{targets} | ocamlformat --enable-outside-detected-project --impl -")))
|}
    |> Str.global_replace (Str.regexp "MODULES") std_modules
    |> Str.global_replace (Str.regexp "DEPS") std_deps
  in
  fun name -> Str.global_replace (Str.regexp "TEMPLATE") name where
;;

let footer names =
  Printf.printf "(cram\n";
  Printf.printf " ;(package noCanren-tests)\n";
  Printf.printf " (deps\n";
  ListLabels.iter names ~f:(Printf.printf "  ./%s_run.exe\n");
  Printf.printf "  ;\n  ))\n";
  flush stdout
;;

let () =
  let path = Sys.argv.(1) in
  let files = Stdlib.Sys.readdir path |> Array.to_list in
  let tail = "_run.ml" in
  let taillen = String.length tail in
  let names =
    ListLabels.filter_map files ~f:(fun s ->
      if String.length s > taillen && (String.equal tail @@ Str.last_chars s taillen)
      then (
        let name = String.sub s 0 (String.length s - taillen) in
        match name with
        | "lorry" -> None
        | _ -> Some name)
      else None)
    |> List.sort String.compare
  in
  (* Array.iter names ~f:Stdio.print_endline; *)
  Printf.printf "%s\n%!" header;
  ListLabels.iter names ~f:(fun name -> Printf.printf "%s%!" (wrap name));
  footer names;
  flush stdout
;;
