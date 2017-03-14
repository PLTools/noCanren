(* let () = Ast_mapper.register "pa_minikanren" (fun _ -> Smart_logger.pa_minikanren) *)
open Migrate_parsetree
open Ast_405

let () = Driver.register ~name:"logger_repr" Versions.ocaml_405
  (fun  _ _ -> Smart_logger.pa_minikanren)

let _ = Driver.run_main ()
