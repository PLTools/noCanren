(*
 * MiniKanren: miniKanren primitives implementation.
 * Copyright (C) 2015
 * Dmitri Boulytchev, Dmitry Kosarev, St.Petersburg State University
 *
 * This software is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License version 2, as published by the Free Software Foundation.
 *
 * This software is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 *
 * See the GNU Library General Public License version 2 for more details
 * (enclosed in the file COPYING).
 *)

(** {1 Implementation of miniKanren primitives} *)

(** {2 Basic modules and types} *)

module type LOGGER = sig
  type t
  type node
  val create: unit -> t
  val make_node: t -> node
  val connect: t -> node -> node -> string -> unit
  val output_plain: filename:string -> t -> unit
  val output_html : filename:string -> string list -> t -> unit
end
module UnitLogger: LOGGER

(** Type of typed logic variable *)
type 'a logic = private Var of 'a var_desc | Value of 'a * ('a -> string)
and  'a var_desc =
  { index: int
  ; mutable reifier: unit -> 'a logic * ('a logic list)
  }


(** Lifting primitive *)
val (!) : 'a -> 'a logic

val embed : {S: ImplicitPrinters.SHOW} -> S.t -> S.t logic

module Show_logic : functor {X : ImplicitPrinters.SHOW} -> sig
                        type t = X.t logic
                        val show : X.t logic -> string
end

val show_logic_naive : 'a logic -> string

(** Type of ligic lists *)
type 'a llist = Nil | Cons of 'a logic * 'a llist logic

val llist_nil : 'a llist logic
(** Infix synonym for [Cons] *)
val (%) : 'a logic -> 'a llist logic -> 'a llist logic

(** [x %< y] is a synonym for [Cons (x, !(Cons (y, !Nil)))] *)
val (%<) : 'a logic -> 'a logic -> 'a llist logic

(** [!< x] is a synonym for [Cons (x, !Nil)] *)
val (!<) : 'a logic -> 'a llist logic

(** [of_list l] converts a regular list into logic one *)
val of_list : {S : ImplicitPrinters.SHOW} -> S.t list -> S.t llist logic

(** [to_value x] converts logic into value; raises [Not_a_value] on a
    non-value case
*)
val to_value : 'a logic -> 'a

(** [to_listk k l] converts logic list [l] into a regular one, calling [k] to
    convert elements, which are not a value *)
val to_listk : ('a llist logic -> 'a list) -> 'a llist logic -> 'a list

(** Exception to raise on a non-value case *)
exception Not_a_value

(** [to_list l] converts logic list [l] into a regular one, raising
    [Not_a_value] on a non-value case *)
val to_list : 'a llist logic -> 'a list

(** Lazy streams *)
module Stream :
  sig

    (** Type of the stream *)
    type 'a t

    (** Lazy constructor *)
    val from_fun : (unit -> 'a t) -> 'a t

    val take: ?n:int -> 'a t -> 'a list
  end

(** State (needed to perform calculations) *)
module State :
sig
  (** State type *)
  type t

  (** Printing helper *)
  val show : t -> string
end

(** Exception to raise on infinine unification result *)
exception Occurs_check

module Make : functor (Logger: LOGGER) -> sig
  module Logger: LOGGER

  type state (* = State.t * Logger.t * Logger.node *)

  val describe_log: state -> Logger.t * Logger.node
  val concrete: state -> State.t

  (** Goal converts a state into a lazy stream of states *)
  type goal = state -> state Stream.t

  val delay_goal: (unit -> goal) -> goal

  (** {2 miniKanren basic primitives} *)

  (** Utility function for logging *)
  val (<=>) : string -> goal -> goal


  (** [call_fresh f] creates a fresh logical variable and passes it to the
    parameter *)
  val call_fresh : ('a logic -> goal) -> goal

  (** [call_fresh_named name f] works the same as [call_fresh f] but adds to
    the log [name] of created logical variable *)
  val call_fresh_named : string -> ('a logic -> goal) -> goal

  (** [x === y] creates a goal, which performs a unifications of
      [x] and [y] *)
  val (===) : 'a logic -> 'a logic -> goal

  (** [x === y] creates a goal, which performs a non-unification check for
      [x] and [y] *)
  val (=/=) : 'a logic -> 'a logic -> goal

  (** [conj s1 s2] creates a goal, which is a conjunction of its arguments *)
  val conj : goal -> goal -> goal

  (** [&&&] is left-associative infix synonym for [conj] *)
  val (&&&) : goal -> goal -> goal

  (** [disj s1 s2] creates a goal, which is a disjunction of its arguments *)
  val disj : goal -> goal -> goal

  (** [|||] is left-associative infix synonym for [disj] *)
  val (|||) : goal -> goal -> goal

  (** [?| [s1; s2; ...; sk]] calculates [s1 ||| s2 ||| ... ||| sk] for a
      non-empty list of goals *)
  val (?|) : goal list -> goal

  (** [conde] is a synonym for [?|] *)
  val conde : goal list -> goal

  (** [?& [s1; s2; ...; sk]] calculates [s1 &&& s2 && ... &&& sk] for a
      non-empty list of goals *)
  val (?&) : goal list -> goal

  (** {2 Top-level running primitives} *)

  (** [run s] runs a state transformer [s] (not necessarily a goal) in
      initial state *)
  val run : Logger.t -> (state -> 'a) -> 'a

  (** [diseq] is a type for disequality constraint *)
  type diseq

  (** [refine s x] refines a logical variable [x] (created with [fresh]) w.r.t.
      state [s] *)
  val refine : State.t -> 'a logic -> 'a logic * diseq

  (** [reify s x] reifies disequality constraint for a given logic variable; the result
      is a list of logic expressions, which given variable should not be equal to *)
  val reify : diseq -> 'a logic -> 'a logic list

  (** [take ?(n=k) s] takes at most [k] first answers from the lazy
      stream [s] (reexported from MKStream for convenience) *)
  val take  : ?n:int -> State.t Stream.t -> State.t list
  val take' : ?n:int -> state Stream.t -> State.t list

  type 'a logic_diseq = 'a logic list

  module ApplyLatest :
  sig
    val two : ('a -> ('a -> 'b) -> 'b) * ('c * 'd -> 'c * 'd)
    val three :
      ('a -> ('a -> 'b) * ('a -> 'c) -> 'b * 'c) *
      ('d * ('e * 'f) -> ('d * 'e) * 'f)
    val apply : ('a -> 'b -> 'c) * ('d -> 'b * 'a) -> 'd -> 'c
    val succ :
      ('a -> 'b -> 'c) * ('d -> 'e * 'f) ->
      ('a -> ('a -> 'g) * 'b -> 'g * 'c) *
      ('h * 'd -> ('h * 'e) * 'f)
  end

  module Convenience : sig
    type 'a reifier = state Stream.t -> int -> (Logger.t * ('a logic * 'a logic list)) list

    (** [succ num f] increments the number of free logic variables in
        a goal; can be used to get rid of ``fresh'' syntax extension *)
    val succ : ('a -> state -> 'z) -> ('c logic -> 'a) -> state -> 'c reifier * 'z

    (** Zero logic parameters *)
    val zero : 'a -> 'a

    (* (\** One to five logic parameter(s) *\) *)
    (* val one   : ('a logic ->                                                 state -> 'z) -> state -> 'a reifier * 'z *)
    (* val two   : ('a logic -> 'b logic ->                                     state -> 'z) -> state -> 'a reifier * ('b reifier * 'z) *)
    (* val three : ('a logic -> 'b logic -> 'c logic ->                         state -> 'z) -> state -> 'a reifier * ('b reifier * ('c reifier * 'z)) *)
    (* val four  : ('a logic -> 'b logic -> 'c logic -> 'd logic ->             state -> 'z) -> state -> 'z *)
    (* val five  : ('a logic -> 'b logic -> 'c logic -> 'd logic -> 'e logic -> state -> 'z) -> state -> 'z *)

    (* (\** One to five logic parameter(s), conventional names *\) *)
    (* val q     : ('a logic ->                                                 state -> 'z) -> state -> 'z *)
    (* val qr    : ('a logic -> 'b logic ->                                     state -> 'z) -> state -> 'z *)
    (* val qrs   : ('a logic -> 'b logic -> 'c logic ->                         state -> 'z) -> state -> 'z *)
    (* val qrst  : ('a logic -> 'b logic -> 'c logic -> 'd logic ->             state -> 'z) -> state -> 'z *)
    (* val pqrst : ('a logic -> 'b logic -> 'c logic -> 'd logic -> 'e logic -> state -> 'z) -> state -> 'z *)

    val run : ('a -> state -> 'c) -> 'a -> 'c
  end

  module Convenience2 : sig
    val zero : 'a -> 'a
    val succ : ('a -> state -> 'b) -> ('c logic -> 'a) -> state -> 'b

    type 'a reifier = int -> ('a logic * 'a logic_diseq) list
    module PolyPairs : sig
      val id : 'a -> 'a


      val one : ('a reifier -> 'b) -> state Stream.t -> 'a logic -> 'b
      val succ :
         (('a -> 'b) -> state Stream.t -> 'c) ->
         ((int -> ('d logic * 'd logic_diseq) list) * 'a -> 'b) ->
         state Stream.t -> 'd logic -> 'c
      val p : (('a -> 'a) -> 'b) -> 'b
    end

    val run: ('a -> state -> 'b * ('b -> 'c)) -> 'a -> 'c
  end

  module Convenience3 :
  sig
    type 'a reifier = int -> (Logger.t * ('a logic * 'a logic list)) list
    type 'a almost_reifier = state Stream.t -> 'a reifier

    module LogicAdder :
    sig
      val zero : 'a -> 'a
      val succ :
        ('a -> state -> 'b) ->
        ('c logic -> 'a) -> state -> 'c almost_reifier * 'b
    end
    val one :
         (('a logic -> state -> 'b) -> state -> 'a almost_reifier * 'b) *
         (('c -> 'd) -> 'c -> 'd) *
         (('e -> ('e -> 'f) -> 'f) * ('g * 'h -> 'g * 'h))

    val succ :
         ('a -> state -> 'b) * ('c -> 'd -> 'e) *
         (('f -> 'g -> 'h) * ('i -> 'j * 'k)) ->
         (('l logic -> 'a) -> state -> 'l almost_reifier * 'b) *
         (('m -> 'c) -> 'm * 'd -> 'e) *
         (('f -> ('f -> 'n) * 'g -> 'n * 'h) * ('o * 'i -> ('o * 'j) * 'k))
    val run :
         ('a -> state -> 'b) * ('c -> 'd -> 'e) *
         (('f -> 'g -> 'd) * ('b -> 'g * 'f)) -> 'a -> 'c -> 'e
  end

  module Convenience4 : sig
    module LogicAdder : sig
      val zero : 'a -> 'a
      val succ :
        ('a -> state -> 'b) ->
        ('c logic -> 'a) -> state -> 'c logic * 'b
      end
   module Refine : sig
      val one : State.t -> 'a logic -> 'a logic * 'a logic_diseq
      val succ :
        (State.t -> 'a -> 'b) ->
        State.t -> 'c logic * 'a -> ('c logic * 'c logic_diseq) * 'b
   end

   val one :
         (('a logic -> state -> 'b) -> state -> 'a logic * 'b) *
         ('c * 'd -> 'c * 'd) *
         (State.t -> 'e logic -> 'e logic * 'e logic_diseq)
   val succ :
         ('a -> state -> 'b) * ('c -> 'd * 'e) * (State.t -> 'f -> 'g) ->
         (('h logic -> 'a) -> state -> 'h logic * 'b) *
         ('i * 'c -> ('i * 'd) * 'e) *
         (State.t -> 'j logic * 'f -> ('j logic * 'j logic_diseq) * 'g)
   val run :
       ('a -> state -> 'b) * ('b -> 'c * state Stream.t) *
       (State.t -> 'c -> 'g) -> 'a -> 'g Stream.t
   end

end
