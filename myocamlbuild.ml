open Ocamlbuild_plugin;;
module Pack = Ocamlbuild_pack

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

let init_js_of_ocaml () =
  let dep = "%.byte" in
  let prod = "%.js" in
  let f env _ =
    let dep = env dep in
    let prod = env prod in
    let link_opts = link_opts prod in
    let tags = tags_of_pathname prod ++ "js_of_ocaml" in
    Cmd (S [A "js_of_ocaml"; A "--no-runtime"; T tags; S link_opts; A "-o"; Px prod; P dep])
  in
  rule "js_of_ocaml: .byte -> .js" ~dep ~prod f;
  (* flag ["js_of_ocaml"; "debug"] (S [A "--pretty"; A "--debug-info"; A "--source-map"]); *)
  flag ["js_of_ocaml"; "pretty"] (A "--pretty");
  flag ["js_of_ocaml"; "debuginfo"] (A "--debug-info");
  flag ["js_of_ocaml"; "noinline"] (A "--no-inline");
  flag ["js_of_ocaml"; "sourcemap"] (A "--source-map");
  pflag ["js_of_ocaml"] "opt" (fun n -> S [A "--opt"; A n]);
  pflag ["js_of_ocaml"] "set" (fun n -> S [A "--set"; A n]);
  ()
;;
open Command;;

let () = dispatch (function
 | Before_rules ->
     ()

 | After_rules ->
     init_js_of_ocaml ();
     (* miniKanren related stuff*)
     (* flag ["ocamldep"; "use_mkshow"] (S [A"-ppopt";A"-I";A"-ppopt";A"plugin"; A"-ppopt";A"mkshow.cmo" ]); *)
     flag ["compile"; "use_pa_minikanren"]
       (S [ A"-ppopt";A"camlp5/pa_minikanren.cmo" ]);

     flag ["compile"; "link_minikanren"]
       (S [ A"-ppopt";A"camlp5/pa_minikanren.cmo"
          ; A"-ppopt";A"-L";A"-ppopt";A"plugin"
	  ]
       );

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

   (* flag ["compile";  "ocaml"; "use_ostap_lib1"] (S[A "-I"; A"lib"; A"bNF3.cmo"]);  *)
   (* flag ["compile";  "ocaml"; "use_ostap_lib"] (S[A"ostap.cmo"]);  *)
   (* flag ["ocamldep"; "ocaml"; "use_pa_log"]  *)
   (*   (S [A"-syntax";A"camlp5o";  *)
   (*           A"-ppopt";A"pr_o.cmo";  *)
   (*           A"-ppopt";P"pa_log.cmo" *)
   (*          ]);  *)

   (* flag ["ocamldep"; "ocaml"; "use_pa_ostap"]  *)
   (*   (S [A"-syntax";A"camlp5o";  *)
   (*      	 A"-ppopt";A"lib/bNF3.cmo"; *)
   (*           A"-ppopt";A"pr_o.cmo";  *)
   (*           A"-ppopt";P"./pa_ostap.cmo" *)
   (*          ]);  *)
   (*  flag ["compile"; "ocaml"; "use_pa_ostap"]  *)
   (*   (S [A"-I";A"lib"; *)
   (*       	 A"-syntax";A"camlp5o";  *)
   (*      	 A"-ppopt";A"lib/bNF3.cmo"; *)
   (*           A"-ppopt";A"pr_o.cmo";  *)
   (*           A"-ppopt";P"./pa_ostap.cmo" *)
   (*          ]);  *)
   (* flag ["compile";  "ocaml"; "use_pa_log"] *)
   (*   (S[A"-syntax";A"camlp5o" *)
   (*         ;A"-ppopt"; P "pa_log.cmo" *)
   (*         ;A"-ppopt"; A"pr_o.cmo" *)
   (*         ]); *)

   (* flag ["compile"; "byte"; "ocaml"; "use_ostap_lib"]   (S[A"ostap.cmo"]); *)
   (* flag ["compile"; "native"; "ocaml"; "use_ostap_lib"]   (S[A"ostap.cmx"]); *)

   (* flag ["link"; "byte"; "ocaml"; "use_ostap_lib"] *)
   (*   (S[A"-package"; A"typeutil"; A"ostap.cmo" *)
   (*         ]); *)
   ()
 | _ -> ()
)
