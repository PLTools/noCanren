open Base

let header =
  {|; THIS FILE IS GENERATED AUTOMATICALLY
(env
 (_
  (flags
   (:standard -rectypes -w -27-33-39))))
|}
;;

let wrap name =
  Printf.sprintf
    "(executable\n\
    \ (name %s_run)\n\
    \ (package noCanren-tests)\n\
    \ (public_name noCanren-tests.%s)\n\
    \ (libraries GT OCanren OCanren.tester)\n\
    \ (modules %s %s_run)\n\
    \ (preprocess\n\
    \  (pps OCanren-ppx.ppx_repr OCanren-ppx.ppx_fresh GT.ppx)))\n\n\
     (rule\n\
    \ (targets %s.ml)\n\
    \ (deps %s.ml2mk.ml)\n\
    \ (action\n\
    \  (run sh -c \"%%{project_root}/src/noCanren.exe %%{deps} -o %%{targets} | \
     ocamlformat --enable-outside-detected-project --impl -\")))\n"
    name
    name
    name
    name
    name
    name
;;

let footer names = Array.iter names ~f:(Stdio.printf "(cram (deps ./%s_run.exe))\n%!")

let () =
  let path = (Sys.get_argv ()).(1) in
  let files = Caml.Sys.readdir path in
  let tail = "_run.ml" in
  let taillen = String.length tail in
  let names =
    Array.filter_map
      ~f:(fun s ->
        if String.length s > taillen && (String.equal tail @@ Str.last_chars s taillen)
        then (
          let name = String.chop_suffix_exn s ~suffix:tail in
          match name with
          | "logic_interpreter" | "lorry" -> None
          | _ -> Some name)
        else None)
      files
  in
  (* Array.iter names ~f:Stdio.print_endline; *)
  Stdio.printf "%s\n%!" header;
  Array.iter names ~f:(fun name -> Stdio.printf "%s\n%!" (wrap name));
  footer names
;;
