module Cfg = Configurator.V1

(*** discovering ***)
let discover_GT_dir cfg =
  let path =
    String.trim @@ Cfg.Process.run_capture_exn cfg "ocamlfind" [ "query"; "GT" ]
  in
  let filename = "path-GT.cfg" in
  let _ = Sys.command (Format.sprintf "echo '%s' > %s" path filename) in
  ()
;;

let discover_OCanren_dir cfg =
  let path =
    String.trim @@ Cfg.Process.run_capture_exn cfg "ocamlfind" [ "query"; "OCanren" ]
  in
  let filename = "path-OCanren.cfg" in
  let _ = Sys.command (Format.sprintf "echo '%s' > %s" path filename) in
  ()
;;

(*** main ***)

let () =
  Cfg.main ~name:"lib" (fun cfg ->
      discover_GT_dir cfg;
      discover_OCanren_dir cfg;
      ())
;;
