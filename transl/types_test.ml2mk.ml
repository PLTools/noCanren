(*type x =
  | B of bool
  | C of int
  | P of string * float*)

type 'a mylist = Nil | Cons of 'a * 'a mylist
