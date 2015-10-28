open Ocamlbuild_plugin;;
open Command;;

let () = dispatch (function
 | Before_rules ->
     ()

 | After_rules ->

   flag ["ocamldep"; "use_mkshow"]
     (S [A"-ppopt";A"-L";A"-ppopt";A"plugin";(* A"-syntax";A"camlp5o -L plugin" *)
	]);

   flag ["compile"; "use_mkshow"]
        (S [ A"-ppopt";A"camlp5/pa_minikanren.cmo"
           ; A"-ppopt";A"-L";A"-ppopt";A"plugin"
	   ]
        );

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
