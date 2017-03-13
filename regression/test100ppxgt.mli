type token_env = int

type 'a logic =
| Var   of GT.int * 'a logic GT.list
| Value of 'a
[@@deriving gt {show} ]

(* type ('a, 'l) llist = Nil | Cons of 'a * 'l
[@@deriving gt {show} ]

type 'a lnat = O | S of 'a
 [@@deriving gt { show } ] *)
