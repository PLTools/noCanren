type var = Var of int

type 'a logic
val inj : 'a -> 'a logic
val prj_exn: 'a logic -> 'a

val logn : ('a, unit, string, unit) format4 -> 'a
val logf : ('a, unit, string, unit) format4 -> 'a

val generic_show : Obj.t -> string
module Env :
sig
  type t
  val empty : unit -> t
  val fresh : t -> 'a * t
  val var : t -> 'a -> int option
  val vars : t -> var list
  val show : t -> string
end
module Subst :
sig
  type t
  val empty : t
  val walk : Env.t -> 'a -> t -> 'a
  val walk' : Env.t -> 'a -> t -> 'a
  val unify : Env.t -> 'a -> 'a -> t option -> t option
  val show : t -> string
end

type state = Env.t * Subst.t
type goal = state -> state Stream.t

val show_st : Env.t * Subst.t -> string
val print_if_var : Env.t -> 'a -> (unit -> string) -> string

class minikanren_string_t :
  object method t_string : Env.t -> string -> string end
class minikanren_int_t : object method t_int : Env.t -> int -> string end
class ['a] minikanren_list_t :
  object
    method c_Cons :
      Env.t ->
      (Env.t, 'a GT.list, string, < a : Env.t -> 'a -> string >) GT.a ->
      (Env.t, 'a, string, < a : Env.t -> 'a -> string >) GT.a ->
      (Env.t, 'a GT.list, string, < a : Env.t -> 'a -> string >) GT.a ->
      string
    method c_Nil :
      Env.t ->
      (Env.t, 'a GT.list, string, < a : Env.t -> 'a -> string >) GT.a ->
      string
    method t_list : (Env.t -> 'a -> string) -> Env.t -> 'a GT.list -> string
  end
val minikanren : ('a, < minikanren : 'b; .. >) GT.t -> 'b
val show_list : Env.t -> (Env.t -> 'a -> string) -> 'a GT.list -> string
val show_int : Env.t -> GT.int -> string
val show_string : Env.t -> GT.string -> GT.string


type ('a, 'l) llist =
  | Nil
  | Cons of 'a * 'l

module LList : sig
  include module type of struct include List end

  (** Synonym for abstract list type *)
  type ('a, 'l) ttt = ('a, 'l) llist

  (** Ground lists (isomorphic to regular ones) *)
  type 'a ground = ('a, 'a ground) ttt
  type 'a t = 'a ground

  val appendo: 'a t logic -> 'a t logic -> 'a t logic -> goal
  val reverso: 'a t logic -> 'a t logic -> goal
  val of_list : 'a list -> 'a t logic

  type nonrec 'a logic  = 'a t logic
end

(** Infix synonym for [Cons] *)
val (%) : 'a logic -> 'a LList.logic -> 'a LList.logic

(** [x %< y] is a synonym for [Cons (x, !(Cons (y, !Nil)))] *)
val (%<) : 'a logic -> 'a logic -> 'a LList.logic

(** [!< x] is a synonym for [Cons (x, !Nil)] *)
val (!<) : 'a logic -> 'a LList.logic

(** [nil] is a synonym for [inj Nil] *)
val nil : 'a LList.logic

val fresh : ('a -> state -> 'c) -> state -> 'c
val (===) : 'a -> 'a -> goal
val conj  : goal -> goal -> goal
val (&&&) : goal -> goal -> goal
val disj : goal -> goal -> goal
val ( ||| ) : ('a -> 'b Stream.t) -> ('a -> 'b Stream.t) -> 'a -> 'b Stream.t
val ( ?| ) : ('a -> 'b Stream.t) list -> 'a -> 'b Stream.t
val ( ?& ) : ('a -> 'a Stream.t) list -> 'a -> 'a Stream.t
val conde : ('a -> 'b Stream.t) list -> 'a -> 'b Stream.t
val call_fresh : ('a -> Env.t * 'b -> 'c) -> Env.t * 'b -> 'c
module Fresh :
sig
  val succ : ('a -> Env.t * 'b -> 'c) -> ('d -> 'a) -> Env.t * 'b -> 'c
  val zero : 'a -> 'a
  val one : ('a -> Env.t * 'b -> 'c) -> Env.t * 'b -> 'c
  val two : ('a -> 'b -> Env.t * 'c -> 'd) -> Env.t * 'c -> 'd
  val three : ('a -> 'b -> 'c -> Env.t * 'd -> 'e) -> Env.t * 'd -> 'e
  val four : ('a -> 'b -> 'c -> 'd -> Env.t * 'e -> 'f) -> Env.t * 'e -> 'f
  val five :
    ('a -> 'b -> 'c -> 'd -> 'e -> Env.t * 'f -> 'g) -> Env.t * 'f -> 'g
  val six :
    ('a -> 'b -> 'c -> 'd -> 'e -> 'f -> Env.t * 'g -> 'h) ->
    Env.t * 'g -> 'h
  val seven :
    ('a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g -> Env.t * 'h -> 'i) ->
    Env.t * 'h -> 'i
  val eight :
    ('a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g -> 'h -> Env.t * 'i -> 'j) ->
    Env.t * 'i -> 'j
end
