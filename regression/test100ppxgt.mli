(*
  to get preprocessed version by PPX use
  make ppx && ocamlfind ppx_tools/rewriter "`ocamlfind query ppx_deriving`/ppx_deriving -deriving-plugin `ocamlfind query ppx_deriving`/ppx_deriving_show.cmo -deriving-plugin _build/ppx/ppx_deriving_gt.cma" -intf regression/test100ppxgt.mli
*)

(* type token_env = int *)

type 'a logic =
| Var   of GT.int * 'a logic GT.list
| Value of 'a
[@@deriving gt {show} ]

(* Next line because we override logic *)
(* val logic : (unit,< show: ('a ->  string) -> 'a logic -> string > ) GT.t *)

(* type ('a, 'l) llist = Nil | Cons of 'a * 'l
[@@deriving gt {show} ]
*)

(* type 'a lnat = O | S of 'a
 [@@deriving gt { show } ] *)
