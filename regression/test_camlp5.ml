(*
  camlp5o -I `ocamlfind query GT` pa_gt.cmo show.cmo gmap.cmo pr_o.cmo regression/test_camlp5.ml
*)
type token_env = int

@type 'a logic =
| Var   of GT.int * 'a logic GT.list * 'a logic
| Value of 'a
with show,gmap


type ('a, 'l) llist = Nil | Cons of 'a * 'l

type 'a lnat = O | S of 'a
