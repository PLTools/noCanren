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

val generic_show : 'a -> string

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
type 'a logic

(** Type [unlogic] is needed to observe logic values in a way when we can't change
 *  them, put them back to evaluating and break something
 *)
type 'a unlogic = [`Var of int * 'a logic list | `Value of 'a ]

val destruct : 'a logic -> 'a unlogic

(** Lifting primitive: create a value without a printer. To inject values with
  * their printers use [inj].
  *)
val (!) : 'a -> 'a logic

(** Lift value and its printer to logic domain *)
val inj   : {S: ImplicitPrinters.SHOW} -> S.t -> S.t logic

(** Synonym for [inj] *)
val embed : {S: ImplicitPrinters.SHOW} -> S.t -> S.t logic

(** Lift value and its printer to logic domain. The same as [inj] but doesn't
 *  abuse modular implicits
 *)
val embed_explicit: ('a -> string) -> 'a -> 'a logic

(* TODO: remove this module because we can use syntax like
   show {Show_int} 5
*)
module Show_logic_explicit : functor (X : ImplicitPrinters.SHOW) -> sig
                        type t = X.t logic
                        val show : X.t logic -> string
end

implicit module Show_logic : functor {X : ImplicitPrinters.SHOW} -> sig
                        type t = X.t logic
                        val show : X.t logic -> string
end

(** Print logic value using printer stored in it *)
val show_logic_naive : 'a logic -> string

(** Printing primitive. Useful in sprintf "%a" like functions *)
val sprintf_logic : unit -> 'a logic -> string
(** Printing with Format module *)
val fprintf_logic : Format.formatter -> 'a logic -> unit

(** Prints logic value with it's constraint in format like
 *  _.num {{ <constraints> }}
 *)
val printf_logic_with_cs : 'a logic -> unit
(** The same for [printf_logic_with_cs] but for using in Format module *)
val fprintf_logic_with_cs : Format.formatter -> 'a logic -> unit

(** Type of logic lists *)
type 'a llist = Nil | Cons of 'a logic * 'a llist logic

(* TODO: remove this module*)
module Show_llist_explicit : functor (X : ImplicitPrinters.SHOW) -> sig
                       type t = X.t llist
                       val show : X.t llist -> string
end

implicit module Show_llist : functor {X : ImplicitPrinters.SHOW} -> sig
                       type t = X.t llist
                       val show : X.t llist -> string
end

(** Empty logic llist *)
val llist_nil : 'a llist logic

(** Returns true when argument is empty list *)
val llist_is_empty : 'a llist -> bool
(** Returns true with argument is logic value which contains empty list *)
val llist_is_empty_logic : 'a llist logic -> bool

val llist_printer : 'a llist -> string
val fprintf_llist : Format.formatter -> 'a llist -> unit

(** Infix synonym for [Cons] *)
val (%) : 'a logic -> 'a llist logic -> 'a llist logic

(** [x %< y] is a synonym for [Cons (x, !(Cons (y, !Nil)))] *)
val (%<) : 'a logic -> 'a logic -> 'a llist logic

(** [!< x] is a synonym for [Cons (x, !Nil)] *)
val (!<) : 'a logic -> 'a llist logic

(** [of_list l] converts a regular list into logic one *)
val of_list : {S : ImplicitPrinters.SHOW} -> S.t list -> S.t llist logic

val of_list_hack : {S : ImplicitPrinters.SHOW} -> S.t list -> S.t llist

(** [to_value x] converts logic into value; raises [Not_a_value] on a
    non-value case
*)
val to_value_exn : 'a logic -> 'a

val is_value : 'a logic -> bool

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

  (** [first_of] find first element which can be evaluated to non-empty stream
     and returns this stream.If no one return result of evaluation of last element  *)
  val first_of : goal list -> goal

  (** [?& [s1; s2; ...; sk]] calculates [s1 &&& s2 && ... &&& sk] for a
      non-empty list of goals *)
  val (?&) : goal list -> goal

  (** {2 Top-level running primitives} *)

  (** [run s] runs a state transformer [s] (not necessarily a goal) in
      initial state *)
  val run : ?logger:Logger.t -> (state -> 'a) -> 'a

  (** [refine s x] refines a logical variable [x] (created with [fresh]) w.r.t.
      state [s] *)
  val refine : State.t -> 'a logic -> 'a logic
(*
  (** [reify s x] reifies disequality constraint for a given logic variable; the result
      is a list of logic expressions, which given variable should not be equal to *)
  val reify : diseq -> 'a logic -> 'a logic list
                                   &*)

  (* (\** [take ?(n=k) s] takes at most [k] first answers from the lazy *)
  (*     stream [s] (reexported from MKStream for convenience) *\) *)
  (* val take  : ?n:int -> State.t Stream.t -> State.t list *)
  val take' : ?n:int -> state Stream.t -> State.t list

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

  (** Allows to run goals and get stream of deep pairs as an answer. For example
   *  run (succ one) (fun q r s -> goal q r s)
   *   : ( ('a result  -> 'b result -> 'c result -> 'z) -> 'z)
   *)
  module ConvenienceCurried :
  sig
    type 'a reifier = (Logger.t * 'a logic) Stream.t
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

  (** Allows to run goals and get stream of deep pairs as an answer. For example
   *  run (succ one) (fun q r s -> goal q r s)
   *   : ( ('a result * ('b result * 'c result)) Stream.t )
   *)
  module ConvenienceStream : sig
    module LogicAdder : sig
      val zero : 'a -> 'a
      val succ :
        ('a -> state -> 'b) ->
        ('c logic -> 'a) -> state -> 'c logic * 'b
      end
   module Refine : sig
      val one : State.t -> 'a logic -> 'a logic
      val succ :
        (State.t -> 'a -> 'b) ->
        State.t -> 'c logic * 'a -> ('c logic) * 'b
   end

   val one :
         unit ->
         (('a logic -> state -> 'b) -> state -> 'a logic * 'b) *
         ('c * 'd -> 'c * 'd) *
         (State.t -> 'e logic -> 'e logic )
   val succ :
         (unit -> ('a -> state -> 'b) * ('c -> 'd * 'e) * (State.t -> 'f -> 'g)) ->
         unit ->
         (('h logic -> 'a) -> state -> 'h logic * 'b) *
         ('i * 'c -> ('i * 'd) * 'e) *
         (State.t -> 'j logic * 'f -> ('j logic) * 'g)
   val run :
       (unit -> ('a -> state -> 'b) *
                ('b -> 'c * state Stream.t) *
                (State.t -> 'c -> 'g)) ->
       'a -> (Logger.t * 'g) Stream.t
   end


  (** {2 Combinators to produce fresh variables} *)
  module Fresh : sig

    (** [succ num f] increments the number of free logic variables in
        a goal; can be used to get rid of ``fresh'' syntax extension *)
    val succ : ('a -> state -> 'b) -> ('c logic -> 'a) -> state -> 'b

    (** Zero logic parameters *)
    val zero : 'a -> 'a

    (** {3 One to five logic parameter(s)} *)
    val one   : ('a logic ->                                                 state -> 'b) -> state -> 'b
    val two   : ('a logic -> 'b logic ->                                     state -> 'c) -> state -> 'c
    val three : ('a logic -> 'b logic -> 'c logic ->                         state -> 'd) -> state -> 'd
    val four  : ('a logic -> 'b logic -> 'c logic -> 'd logic ->             state -> 'e) -> state -> 'e
    val five  : ('a logic -> 'b logic -> 'c logic -> 'd logic -> 'e logic -> state -> 'f) -> state -> 'f

    (** {3 One to five logic parameter(s), conventional names} *)
    val q     : ('a logic ->                                                 state -> 'b) -> state -> 'b
    val qr    : ('a logic -> 'b logic ->                                     state -> 'c) -> state -> 'c
    val qrs   : ('a logic -> 'b logic -> 'c logic ->                         state -> 'd) -> state -> 'd
    val qrst  : ('a logic -> 'b logic -> 'c logic -> 'd logic ->             state -> 'e) -> state -> 'e
    val pqrst : ('a logic -> 'b logic -> 'c logic -> 'd logic -> 'e logic -> state -> 'f) -> state -> 'f

  end

  module Std : sig
    val list_cons : 'a llist logic  -> 'a logic -> 'a llist logic -> goal
    val list_hd   : 'a llist logic  -> 'a logic -> goal
    val list_tail : 'a llist logic  -> 'a llist logic -> goal

    val appendo : 'a llist logic -> 'a llist logic -> 'a llist logic -> goal
    val foldo : ('b logic -> 'a logic -> 'b logic -> goal) -> 'b logic -> 'a llist logic
                -> 'b logic -> goal
  end
end
