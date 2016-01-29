open Ast_mapper

let () = register "minikanren_logger" Smart_logger.smart_logger

let () = register "logger_repr" (fun _ -> Ppx_repr.mapper)
