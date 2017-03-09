@type token_env = int with show,gmap

type 'a logic =
| Var   of GT.int * 'a logic GT.list
| Value of 'a



type ('a, 'l) llist = Nil | Cons of 'a * 'l

type 'a lnat = O | S of 'a
