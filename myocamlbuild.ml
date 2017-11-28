open Printf
open Ocamlbuild_plugin;;

let ends_with ~suffix s =
  let slen = String.length suffix in
  (String.length s >= slen) && (Str.last_chars s slen = suffix)

let fold f =
  let l = ref [] in
  (try while true do l @:= [f ()] done with _ -> ());
  !l

let split_comma = Str.split_delim (Str.regexp ",")

let fold_pflag scan =
  List.fold_left
    (fun acc x -> try split_comma (scan x (fun x -> x)) @ acc with _ -> acc)
    []

let ocamlfind cmd f =
  let p = Printf.sprintf in
  let cmd = List.map (p "\"%s\"") cmd in
  let cmd = p "ocamlfind query %s" (String.concat " " cmd) in
  Pack.My_unix.run_and_open cmd (fun ic -> fold (fun () -> f ic))

let link_opts prod =
  let (all_pkgs, predicates) =
    let tags = Tags.elements (tags_of_pathname prod) in
    let pkgs = fold_pflag (fun x -> Scanf.sscanf x "package(%[^)])") tags in
    let predicates = fold_pflag (fun x -> Scanf.sscanf x "predicate(%[^)])") tags in
    ("js_of_ocaml" :: pkgs, predicates)
  in

  (* Findlib usualy set pkg_* predicate for all selected packages *)
  (* It doesn't do it with 'query' command, we have to it manualy. *)
  let cmd = "-format" :: "pkg_%p" :: "-r" :: all_pkgs in
  let predicates_pkgs = ocamlfind cmd (fun ic -> input_line ic) in

  let all_predicates = String.concat "," ("javascript" :: predicates @ predicates_pkgs) in

  (* query findlib for linking option *)
  let cmd = "-o-format" :: "-r" :: "-predicates" :: all_predicates :: all_pkgs in
  ocamlfind cmd (fun ic -> A (input_line ic))


(*let library_index = Hashtbl.create 32
let package_index = Hashtbl.create 32
let hidden_packages: string list ref = ref []

let hide_package_contents package = hidden_packages := package :: !hidden_packages*)

(*module Ocaml_dependencies_input = struct
  let fold_dependencies = Pack.Resource.Cache.fold_dependencies
  let fold_libraries f = Hashtbl.fold f library_index
  let fold_packages f = Hashtbl.fold f package_index
end
module Ocaml_dependencies = Pack.Ocaml_dependencies.Make(Ocaml_dependencies_input)
*)


open Command;;

let () = dispatch (function
 | Before_rules ->
    (* Options.ocamlopt := S[A"ocamlfind"; A"opt"; A"-dsource"]; *)
    Options.ocamlc   := S[A"ocamlfind"; A"c"
      (* ;   A"-dsource" *)
      ; A"-verbose"
      ];
    ()
 | After_rules ->
     ocaml_lib "src/OCanren";
     (* miniKanren related stuff*)
     (* flag ["ocamldep"; "use_mkshow"] (S [A"-ppopt";A"-I";A"-ppopt";A"plugin"; A"-ppopt";A"mkshow.cmo" ]); *)
     flag ["compile"; "use_pa_ocanren"]
       (S [ A"-ppopt";A"camlp5/pa_ocanren.cmo"]);

     flag ["compile"; "use_time_log"]
       (S [ A"-package"; A"logger.syntax"; A"-ppopt";A"-LOG"]);

     flag ["hack_pr_o"; "compile"] (S[A"-ppopt"; A"pr_o.cmo"]);
     (* flag ["compile"; "link_minikanren"] *)
     (*   (S [ A"-ppopt";A"camlp5/pa_minikanren.cmo" *)
     (*      ; A"-ppopt";A"-L";A"-ppopt";A"plugin" *)
     (*      ] *)
     (*   ); *)

     flag ["ocaml";"compile";"use_ocanren_modules"] (S[A"-I"; A"src";A"-I"; A"regression"]);
     flag ["ocaml";"link";"link_ocanren_modules"] (S[A"-I"; A"src"; A"MiniKanren.cmxa"; A"-I"; A"regression"; A"tester.cmx"]);

     flag ["ocaml";"compile";"native";"keep_asm"] (S[A"-S"]);
     flag ["ocaml";"compile";"dsource"] (S[A"-dsource"]);
     flag ["ocaml";"compile";"dparsetree"] (S[A"-dparsetree"]);
     pflag ["ocaml"; "dep"]     "plugin" (fun s -> S [A "-plugin"; A s]) ;
     pflag ["ocaml"; "compile"] "plugin" (fun s -> S [A "-plugin"; A s]) ;

     (* cppo-related stuff *)
     let cppo_rules ext =
       let dep   = "%(name).cppo"-.-ext
       and prod1 = "%(name: <*> and not <*.cppo>)"-.-ext
       and prod2 = "%(name: <**/*> and not <**/*.cppo>)"-.-ext in
       let cppo_rule prod env _build =
         let dep = env dep in
         let prod = env prod in
         let tags = tags_of_pathname prod ++ "cppo" in
         Cmd (S[A "cppo"; T tags; S [A "-o"; P prod]; P dep ])
       in
       rule ("cppo: *.cppo."-.-ext^" -> *."-.-ext)  ~dep ~prod:prod1 (cppo_rule prod1);
       rule ("cppo: **/*.cppo."-.-ext^" -> **/*."-.-ext)  ~dep ~prod:prod2 (cppo_rule prod2);
     in
     List.iter cppo_rules ["ml"; "mli"];

     pflag ["cppo"] "cppo_D" (fun s -> S [A "-D"; A s]) ;
     pflag ["cppo"] "cppo_U" (fun s -> S [A "-U"; A s]) ;
     pflag ["cppo"] "cppo_I" (fun s ->
       if Pathname.is_directory s then S [A "-I"; P s]
       else S [A "-I"; P (Pathname.dirname s)]
     );
     pdep ["cppo"] "cppo_I" (fun s ->
       if Pathname.is_directory s then [] else [s]) ;
     flag ["cppo"; "cppo_q"] (A "-q") ;
     flag ["cppo"; "cppo_s"] (A "-s") ;
     flag ["cppo"; "cppo_n"] (A "-n") ;
     pflag ["cppo"] "cppo_x" (fun s -> S [A "-x"; A s]);
     pflag ["cppo"] "cppo_V" (fun s -> S [A "-V"; A s]);
     flag ["cppo"; "cppo_V_OCAML"] & S [A "-V"; A ("OCAML:" ^ Sys.ocaml_version)];


    (* ml2mk conversion rules stuff *)
    let ml2mk_rules ext =
      let dep   = "%(name).ml2mk"-.-ext
      and prod1 = "%(name: <*> and not <*.ml2mk> and not <*.cppo>)"-.-ext
      and prod2 = "%(name: <**/*> and not <**/*.ml2mk> and not <**/*.cppo>)"-.-ext in
      let ml2mk_rule prod env _build =
        let dep = env dep in
        let prod = env prod in
        let tags = tags_of_pathname prod ++ "ml2mk" in
        Cmd (S[A "transl/ml2mk_pp.native"; T tags; S [A"-rectypes"; A"-dtypedtree"; A "-I";A"src";A"-o";P prod]; P dep ])
      in
      rule ("ml2mk: *.ml2mk."-.-ext^" -> *."-.-ext)        ~dep ~prod:prod1 (ml2mk_rule prod1);
      rule ("ml2mk: **/*.ml2mk."-.-ext^" -> **/*."-.-ext)  ~dep ~prod:prod2 (ml2mk_rule prod2);
    in
    List.iter ml2mk_rules ["ml"; "mli"];

     flag ["compile"; "short_paths"] & S [A "-short-paths"];
     flag ["compile"; "backtrack"] & S [A "-backtrack"];

   ()
 | _ -> ()
)
