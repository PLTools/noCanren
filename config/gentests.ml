open Base

let header =
  {|; THIS FILE IS GENERATED AUTOMATICALLY

(env
 (_
  (flags
   (:standard -rectypes -w -27-33-39))))|}
;;

let wrap name =
  let where =
    {|
(executable
 (name TEMPLATE_run)
 ;(package noCanren-tests)
 ;(public_name noCanren-tests.TEMPLATE)
 (libraries GT OCanren OCanren.tester)
 (modules TEMPLATE TEMPLATE_run)
 (preprocess
  (pps
    OCanren-ppx.ppx_repr
    OCanren-ppx.ppx_fresh
    OCanren-ppx.ppx_distrib
    GT.ppx_all)))

(rule
 (targets TEMPLATE.ml)
 (deps
  (:exec %{project_root}/src/noCanren.exe)
  (:input TEMPLATE.ml2mk.ml))
 (action
  (run
   sh
   -c
   "%{exec} -w -8 %{input} -o %{targets} | ocamlformat --enable-outside-detected-project --impl -")))
|}
  in
  Str.global_replace (Str.regexp "TEMPLATE") name where
;;

let footer names =
  Stdio.printf "(cram\n";
  Stdio.printf " ;(package noCanren-tests)\n";
  Stdio.printf " (deps\n";
  Array.iter names ~f:(Stdio.printf "  ./%s_run.exe\n");
  Stdio.printf "))\n"
;;

let () =
  let path = (Sys.get_argv ()).(1) in
  let files = Caml.Sys.readdir path in
  let tail = "_run.ml" in
  let taillen = String.length tail in
  let names =
    Array.filter_map files ~f:(fun s ->
        if String.length s > taillen && (String.equal tail @@ Str.last_chars s taillen)
        then (
          let name = String.chop_suffix_exn s ~suffix:tail in
          match name with
          | "logic_interpreter" | "lorry" -> None
          | _ -> Some name)
        else None)
    |> Array.sorted_copy ~compare:String.compare
  in
  (* Array.iter names ~f:Stdio.print_endline; *)
  Stdio.printf "%s\n%!" header;
  Array.iter names ~f:(fun name -> Stdio.printf "%s%!" (wrap name));
  footer names
;;
