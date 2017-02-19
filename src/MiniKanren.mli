(*
 * MiniKanren: miniKanren implementation.
 * Copyright (C) 2015-2016
 * Dmitri Boulytchev, Dmitry Kosarev, Alexey Syomin,
 * St.Petersburg State University, JetBrains Research
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

(** {3 Lazy streams} *)

module Stream :
  sig
    type 'a t
    val is_empty : 'a t -> bool
    val from_fun : (unit -> 'a t) -> 'a t
    val retrieve : ?n:int -> 'a t -> 'a list * 'a t
    val take : ?n:int -> 'a t -> 'a list
    val hd : 'a t -> 'a
    val tl : 'a t -> 'a t
    val map : ('a -> 'b) -> 'a t -> 'b t
    val iter : ('a -> unit) -> 'a t -> unit
  end

val generic_show : 'a -> string

(** {3 States and goals} *)

(** A state *)
module State : sig type t val show : t -> string end

(** Goal converts a state into lazy stream of states *)
type goal = State.t -> State.t Stream.t

(** {3 Logics} *)
(** Type [('a, 'b) fancy] is a fence between logic values and normal values.
 *)
type ('a, 'b) fancy

(** A type of abstract logic values *)
type 'a logic =
    Var of int * 'a logic list
  | Value of 'a

val bprintf_logic : Buffer.t -> ('a -> unit) -> 'a logic -> unit
val show_logic : ('a -> string) -> 'a logic -> string
val logic :
  (unit, < show : ('a -> string) -> 'a logic -> string;
  html : ('a -> HTML.viewer) -> 'a logic -> HTML.viewer;
  eq : ('a -> 'a -> bool) -> 'a logic -> 'a logic -> bool;
  compare :
    ('a -> 'a -> GT.comparison) -> 'a logic -> 'a logic -> GT.comparison;
  foldl : ('syn -> 'a -> 'syn) -> 'syn -> 'a logic -> 'syn;
  foldr : ('syn -> 'a -> 'syn) -> 'syn -> 'a logic -> 'syn;
  gmap : ('a -> 'sa) -> 'a logic -> 'sa logic >)
    GT.t

val lift : 'a -> ('a, 'a) fancy
val inj : ('a, 'b) fancy -> ('a, 'b logic) fancy

(** A synonym for [inj] *)
val (!!) : ('a, 'b) fancy -> ('a, 'b) fancy

(** {2 miniKanren basic primitives} *)

(** [call_fresh f] creates a fresh logical variable and passes it to the
    parameter *)
val call_fresh : (('a, 'b) fancy -> State.t -> 'r) -> State.t -> 'r

(** [x === y] creates a goal, which performs a unifications of
    [x] and [y] *)
val (===) : ('a, 'b) fancy -> ('a, 'b) fancy -> goal

(** [x =/= y] creates a goal, which introduces a disequality constraint for
    [x] and [y] *)
val (=/=) : ('a, 'b) fancy -> ('a, 'b) fancy -> goal

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

(** {2 Some predefined goals} *)

(** [success] always succeeds *)
val success : goal

(** [failure] always fails *)
val failure : goal

(** {2 Combinators to produce fresh variables} *)
module Fresh :
  sig
    val succ : ('a -> State.t -> 'b) -> ((_, _) fancy -> 'a) -> State.t -> 'b
    val zero : 'a -> 'a
    val one : (_ fancy -> State.t -> 'r) -> State.t -> 'r
    val two : (_ fancy -> _ fancy -> State.t -> 'r) -> State.t -> 'r
    val three :
      (_ fancy -> _ fancy -> _ fancy -> State.t -> 'r) -> State.t -> 'r
    val four :
      (_ fancy -> _ fancy -> _ fancy -> _ fancy -> State.t -> 'r) ->
        State.t -> 'r
    val five :
      (_ fancy -> _ fancy -> _ fancy -> _ fancy -> _ fancy -> State.t ->
         'r) ->
        State.t -> 'r
    val q : (_ fancy -> State.t -> 'r) -> State.t -> 'r
    val qr : (_ fancy -> _ fancy -> State.t -> 'r) -> State.t -> 'r
    val qrs :
      (_ fancy -> _ fancy -> _ fancy -> State.t -> 'r) -> State.t -> 'r
    val qrst :
      (_ fancy -> _ fancy -> _ fancy -> _ fancy -> State.t -> 'r) ->
        State.t -> 'r
    val pqrst :
      (_ fancy -> _ fancy -> _ fancy -> _ fancy -> _ fancy -> State.t ->
         'r) ->
        State.t -> 'r
  end

(** {2 Top-level running primitives} *)

(** [run n g h] runs a goal [g] with [n] logical parameters and passes refined
    results to the handler [h]. The number of parameters is encoded using variadic
    machinery {a la} Danvy and represented by a number of predefined numerals and
    successor function (see below). The refinement replaces each variable, passed
    to [g], with the stream of values, associated with that variables as the goal
    succeeds.

    Examples:

    - [run one        (fun q   -> q === !5)              (fun qs    -> ]{i here [q]s     --- a stream of all values, associated with the variable [q]}[)]
    - [run two        (fun q r -> q === !5 ||| r === !6) (fun qs rs -> ]{i here [qs], [rs] --- streams of all values, associated with the variable [q] and [r], respectively}[)]
    - [run (succ one) (fun q r -> q === !5 ||| r === !6) (fun qs rs -> ]{i the same as the above}[)]
 *)
val run :
  (unit ->
     ('a -> State.t -> 'c) * ('d -> 'e -> 'f) *
       (('g -> 'h -> 'e) * ('c -> 'h * 'g))) ->
    'a -> 'd -> 'f

type var_checker
type ('a, 'b) reification_rez =
    Final of 'a
  | HasFreeVars of var_checker * ('a, 'b) fancy

(** Some type to refine a stream of states into the stream of answers (w.r.t. some known
    logic variable *)
type ('a, 'b) refiner = State.t Stream.t -> ('a, 'b) reification_rez Stream.t


(** Successor function *)
val succ :
  (unit ->
     ('a -> State.t -> 'b) * ('c -> 'd -> 'e) *
       (('f -> 'g -> 'h) * ('i -> 'j * 'k))) ->
    unit ->
    (('l -> 'a) -> State.t -> ('l, 'm) refiner * 'b) *
      (('n -> 'c) -> 'n * 'd -> 'e) *
      (('f -> ('f -> 'o) * 'g -> 'o * 'h) * ('p * 'i -> ('p * 'j) * 'k))



(** {3 Predefined numerals (one to five)} *)

val one :
  unit ->
    ((('a, 'c) fancy -> State.t -> 'b) -> State.t -> ('a, 'c) refiner * 'b) *
      (('d -> 'e) -> 'd -> 'e) * (('f -> ('f -> 'g) -> 'g) * ('h -> 'h))

val two :
  unit ->
    ((('a, 'd) fancy -> ('b, 'e) fancy -> State.t -> 'c) -> State.t ->
       ('a, 'd) refiner * (('b, 'e) refiner * 'c)) *
      (('f -> 'g -> 'h) -> 'f * 'g -> 'h) *
      (('i -> ('i -> 'j) * ('i -> 'k) -> 'j * 'k) *
         ('l * ('m * 'n) -> ('l * 'm) * 'n))

val three :
  unit ->
    ((('a, 'e) fancy -> ('b, 'f) fancy -> ('c, 'g) fancy -> State.t -> 'd) ->
       State.t ->
       ('a, 'e) refiner * (('b, 'f) refiner * (('c, 'g) refiner * 'd))) *
      (('h -> 'i -> 'j -> 'k) -> 'h * ('i * 'j) -> 'k) *
      (('l -> ('l -> 'm) * (('l -> 'n) * ('l -> 'o)) -> 'm * ('n * 'o)) *
         ('p * ('q * ('r * 's)) -> ('p * ('q * 'r)) * 's))

val four :
  unit ->
    ((('a, 'f) fancy -> ('b, 'g) fancy -> ('c, 'h) fancy -> ('d, 'i) fancy ->
        State.t -> 'e) ->
       State.t ->
       ('a, 'f) refiner *
         (('b, 'g) refiner * (('c, 'h) refiner * (('d, 'i) refiner * 'e)))) *
      (('j -> 'k -> 'l -> 'm -> 'n) -> 'j * ('k * ('l * 'm)) -> 'n) *
      (('o -> ('o -> 'p) * (('o -> 'q) * (('o -> 'r) * ('o -> 's))) ->
          'p * ('q * ('r * 's))) *
         ('t * ('u * ('v * ('w * 'x))) -> ('t * ('u * ('v * 'w))) * 'x))

val five :
  unit ->
    ((('a, 'g) fancy -> ('b, 'h) fancy -> ('c, 'i) fancy -> ('d, 'j) fancy ->
        ('e, 'k) fancy -> State.t -> 'f) ->
       State.t ->
       ('a, 'g) refiner *
         (('b, 'h) refiner *
            (('c, 'i) refiner *
               (('d, 'j) refiner * (('e, 'k) refiner * 'f))))) *
      (('l -> 'm -> 'n -> 'o -> 'p -> 'q) -> 'l * ('m * ('n * ('o * 'p))) ->
         'q) *
      (('r ->
          ('r -> 's) *
            (('r -> 't) * (('r -> 'u) * (('r -> 'v) * ('r -> 'w)))) ->
          's * ('t * ('u * ('v * 'w)))) *
         ('x * ('y * ('z * ('a1 * ('b1 * 'c1)))) ->
            ('x * ('y * ('z * ('a1 * 'b1)))) * 'c1))

(** {3 The same numerals with conventional names} *)

val q :
  unit ->
    ((('a, 'c) fancy -> State.t -> 'b) -> State.t -> ('a, 'c) refiner * 'b) *
      (('d -> 'e) -> 'd -> 'e) * (('f -> ('f -> 'g) -> 'g) * ('h -> 'h))

val qr :
  unit ->
    ((('a, 'd) fancy -> ('b, 'e) fancy -> State.t -> 'c) -> State.t ->
       ('a, 'd) refiner * (('b, 'e) refiner * 'c)) *
      (('f -> 'g -> 'h) -> 'f * 'g -> 'h) *
      (('i -> ('i -> 'j) * ('i -> 'k) -> 'j * 'k) *
         ('l * ('m * 'n) -> ('l * 'm) * 'n))

val qrs :
  unit ->
    ((('a, 'e) fancy -> ('b, 'f) fancy -> ('c, 'g) fancy -> State.t -> 'd) ->
       State.t ->
       ('a, 'e) refiner * (('b, 'f) refiner * (('c, 'g) refiner * 'd))) *
      (('h -> 'i -> 'j -> 'k) -> 'h * ('i * 'j) -> 'k) *
      (('l -> ('l -> 'm) * (('l -> 'n) * ('l -> 'o)) -> 'm * ('n * 'o)) *
         ('p * ('q * ('r * 's)) -> ('p * ('q * 'r)) * 's))

val qrst :
  unit ->
    ((('a, 'f) fancy -> ('b, 'g) fancy -> ('c, 'h) fancy -> ('d, 'i) fancy ->
        State.t -> 'e) ->
       State.t ->
       ('a, 'f) refiner *
         (('b, 'g) refiner * (('c, 'h) refiner * (('d, 'i) refiner * 'e)))) *
      (('j -> 'k -> 'l -> 'm -> 'n) -> 'j * ('k * ('l * 'm)) -> 'n) *
      (('o -> ('o -> 'p) * (('o -> 'q) * (('o -> 'r) * ('o -> 's))) ->
          'p * ('q * ('r * 's))) *
         ('t * ('u * ('v * ('w * 'x))) -> ('t * ('u * ('v * 'w))) * 'x))

val pqrst :
  unit ->
    ((('a, 'g) fancy -> ('b, 'h) fancy -> ('c, 'i) fancy -> ('d, 'j) fancy ->
        ('e, 'k) fancy -> State.t -> 'f) ->
       State.t ->
       ('a, 'g) refiner *
         (('b, 'h) refiner *
            (('c, 'i) refiner *
               (('d, 'j) refiner * (('e, 'k) refiner * 'f))))) *
      (('l -> 'm -> 'n -> 'o -> 'p -> 'q) -> 'l * ('m * ('n * ('o * 'p))) ->
         'q) *
      (('r ->
          ('r -> 's) *
            (('r -> 't) * (('r -> 'u) * (('r -> 'v) * ('r -> 'w)))) ->
          's * ('t * ('u * ('v * 'w)))) *
         ('x * ('y * ('z * ('a1 * ('b1 * 'c1)))) ->
            ('x * ('y * ('z * ('a1 * 'b1)))) * 'c1))


(** {2 Building reifiers compositionally } *)

module type T1 = sig type 'a t val fmap : ('a -> 'b) -> 'a t -> 'b t end
module type T2 =
  sig
    type ('a, 'b) t
    val fmap : ('a -> 'c) -> ('b -> 'd) -> ('a, 'b) t -> ('c, 'd) t
  end
module type T3 =
  sig
    type ('a, 'b, 'c) t
    val fmap :
      ('a -> 'q) -> ('b -> 'r) -> ('c -> 's) -> ('a, 'b, 'c) t ->
        ('q, 'r, 's) t
  end

module Fmap1 (T : T1) :
  sig
    val distrib : ('a, 'b) fancy T.t -> ('a T.t, 'b T.t) fancy
    val reifier :
      (var_checker -> ('a, 'b) fancy -> 'b) -> var_checker ->
        ('a T.t, 'b T.t logic as 'r) fancy -> 'r
  end
module Fmap2 (T : T2) :
  sig
    val distrib :
      (('a, 'c) fancy, ('b, 'd) fancy) T.t ->
        (('a, 'b) T.t, ('c, 'd) T.t) fancy
    val reifier :
      (var_checker -> ('a, 'b) fancy -> 'b) ->
        (var_checker -> ('c, 'd) fancy -> 'd) -> var_checker ->
        (('a, 'c) T.t, ('b, 'd) T.t logic as 'r) fancy -> 'r
  end
module Fmap3 (T : T3) :
  sig
    type ('a, 'b, 'c) t = ('a, 'b, 'c) T.t
    val distrib :
      (('a, 'b) fancy, ('c, 'd) fancy, ('e, 'f) fancy) t ->
        (('a, 'c, 'e) t, ('b, 'd, 'f) t) fancy
    val reifier :
      (var_checker -> ('a, 'b) fancy -> 'b) ->
        (var_checker -> ('c, 'd) fancy -> 'd) ->
        (var_checker -> ('e, 'f) fancy -> 'f) -> var_checker ->
        (('a, 'c, 'e) T.t, ('b, 'd, 'f) T.t logic as 'r) fancy -> 'r
  end

module ManualReifiers :
  sig
    val int_reifier : var_checker -> (int, int logic) fancy -> int logic
    val string_reifier :
      var_checker -> (string, string logic) fancy -> string logic
    val pair_reifier :
      (var_checker -> ('a, 'b) fancy -> 'b) ->
        (var_checker -> ('c, 'd) fancy -> 'd) -> var_checker ->
        ('a * 'c, ('b * 'd) logic as 'r) fancy -> 'r
  end

(** {2 Standart relational library } *)

(** {3 Support for some predefined types (lists, nats, bools etc.)} *)

(** Abstract list type *)
type ('a, 'l) llist =
    Nil
  | Cons of 'a * 'l
class type virtual ['a, 'ia, 'sa, 'l, 'il, 'sl, 'inh, 'syn] llist_tt =
  object
    method c_Nil :
      'inh ->
        ('inh, ('a, 'l) llist, 'syn, < a : 'ia -> 'a -> 'sa; l : 'il -> 'l -> 'sl >)
          GT.a ->
        'syn
    method c_Cons :
      'inh ->
        ('inh, ('a, 'l) llist, 'syn, < a : 'ia -> 'a -> 'sa; l : 'il -> 'l -> 'sl >)
          GT.a ->
        ('ia, 'a, 'sa, < a : 'ia -> 'a -> 'sa; l : 'il -> 'l -> 'sl >) GT.a ->
        ('il, 'l, 'sl, < a : 'ia -> 'a -> 'sa; l : 'il -> 'l -> 'sl >) GT.a ->
        'syn
    method t_llist :
      ('ia -> 'a -> 'sa) -> ('il -> 'l -> 'sl) -> 'inh -> ('a, 'l) llist ->
        'syn
  end
val llist :
  (('ia -> 'a -> 'sa) -> ('il -> 'l -> 'sl) ->
    ('a, 'ia, 'sa, 'l, 'il, 'sl, 'inh, 'syn) #llist_tt -> 'inh ->
    ('a, 'l) llist -> 'syn, < show : ('a -> string) -> ('l -> string) -> ('a, 'l) llist -> string;
  gmap : ('a -> 'sa) -> ('l -> 'sl) -> ('a, 'l) llist -> ('sa, 'sl) llist;
  html :
    ('a -> HTML.viewer) -> ('l -> HTML.viewer) -> ('a, 'l) llist ->
      HTML.viewer;
  eq :
    ('a -> 'a -> bool) -> ('l -> 'l -> bool) -> ('a, 'l) llist ->
      ('a, 'l) llist -> bool;
  compare :
    ('a -> 'a -> GT.comparison) -> ('l -> 'l -> GT.comparison) ->
      ('a, 'l) llist -> ('a, 'l) llist -> GT.comparison;
  foldl :
    ('syn -> 'a -> 'syn) -> ('syn -> 'l -> 'syn) -> 'syn -> ('a, 'l) llist ->
      'syn;
  foldr :
    ('syn -> 'a -> 'syn) -> ('syn -> 'l -> 'syn) -> 'syn -> ('a, 'l) llist ->
      'syn >)
    GT.t
class virtual ['a, 'ia, 'sa, 'l, 'il, 'sl, 'inh, 'syn] llist_t :
  object
    method virtual c_Nil :
      'inh ->
        ('inh, ('a, 'l) llist, 'syn, < a : 'ia -> 'a -> 'sa; l : 'il -> 'l -> 'sl >)
          GT.a ->
        'syn
    method virtual c_Cons :
      'inh ->
        ('inh, ('a, 'l) llist, 'syn, < a : 'ia -> 'a -> 'sa; l : 'il -> 'l -> 'sl >)
          GT.a ->
        ('ia, 'a, 'sa, < a : 'ia -> 'a -> 'sa; l : 'il -> 'l -> 'sl >) GT.a ->
        ('il, 'l, 'sl, < a : 'ia -> 'a -> 'sa; l : 'il -> 'l -> 'sl >) GT.a ->
        'syn
    method t_llist :
      ('ia -> 'a -> 'sa) -> ('il -> 'l -> 'sl) -> 'inh -> ('a, 'l) llist ->
        'syn
  end
class type ['a, 'l] show_llist_env_tt = object  end
class type ['a, 'sa, 'l, 'sl] gmap_llist_env_tt = object  end
class type ['a, 'l] html_llist_env_tt = object  end
class type ['a, 'l] eq_llist_env_tt = object  end
class type ['a, 'l] compare_llist_env_tt = object  end
class type ['a, 'l, 'syn] foldl_llist_env_tt = object  end
class type ['a, 'l, 'syn] foldr_llist_env_tt = object  end
class ['a, 'l] show_proto_llist :
  ('a, 'l) show_llist_env_tt ref ->
    object
      inherit ['a, unit, string, 'l, unit, string, unit, string] llist_tt
    end
class ['a, 'sa, 'l, 'sl] gmap_proto_llist :
  ('a, 'sa, 'l, 'sl) gmap_llist_env_tt ref ->
    object
      inherit ['a, unit, 'sa, 'l, unit, 'sl, unit, ('sa, 'sl) llist] llist_tt
    end
class ['a, 'l] html_proto_llist :
  ('a, 'l) html_llist_env_tt ref ->
    object
      inherit
        ['a, unit, HTML.viewer, 'l, unit, HTML.viewer, unit, HTML.viewer]
           llist_tt
      method attribute : ('a, 'l) llist -> string
      method cname : string -> string
    end
class ['a, 'l] eq_proto_llist :
  ('a, 'l) eq_llist_env_tt ref ->
    object
      inherit ['a, 'a, bool, 'l, 'l, bool, ('a, 'l) llist, bool] llist_tt
    end
class ['a, 'l] compare_proto_llist :
  ('a, 'l) compare_llist_env_tt ref ->
    object
      inherit
        ['a, 'a, GT.comparison, 'l, 'l, GT.comparison, ('a, 'l) llist,
         GT.comparison]
           llist_tt
    end
class ['a, 'l, 'syn] foldl_proto_llist :
  ('a, 'l, 'syn) foldl_llist_env_tt ref ->
    object inherit ['a, 'syn, 'syn, 'l, 'syn, 'syn, 'syn, 'syn] llist_tt end
class ['a, 'l, 'syn] foldr_proto_llist :
  ('a, 'l, 'syn) foldr_llist_env_tt ref ->
    object inherit ['a, 'syn, 'syn, 'l, 'syn, 'syn, 'syn, 'syn] llist_tt end
class ['a, 'l] show_llist_t :
  object
    inherit ['a, unit, string, 'l, unit, string, unit, string] llist_tt
    inherit ['a, 'l] show_llist_env_tt
  end
class ['a, 'sa, 'l, 'sl] gmap_llist_t :
  object
    inherit ['a, unit, 'sa, 'l, unit, 'sl, unit, ('sa, 'sl) llist] llist_tt
    inherit ['a, 'sa, 'l, 'sl] gmap_llist_env_tt
  end
class ['a, 'l] html_llist_t :
  object
    inherit
      ['a, unit, HTML.viewer, 'l, unit, HTML.viewer, unit, HTML.viewer]
         llist_tt
    inherit ['a, 'l] html_llist_env_tt
    method attribute : ('a, 'l) llist -> string
    method cname : string -> string
  end
class ['a, 'l] eq_llist_t :
  object
    inherit ['a, 'a, bool, 'l, 'l, bool, ('a, 'l) llist, bool] llist_tt
    inherit ['a, 'l] eq_llist_env_tt
  end
class ['a, 'l] compare_llist_t :
  object
    inherit
      ['a, 'a, GT.comparison, 'l, 'l, GT.comparison, ('a, 'l) llist,
       GT.comparison]
         llist_tt
    inherit ['a, 'l] compare_llist_env_tt
  end
class ['a, 'l, 'syn] foldl_llist_t :
  object
    inherit ['a, 'syn, 'syn, 'l, 'syn, 'syn, 'syn, 'syn] llist_tt
    inherit ['a, 'l, 'syn] foldl_llist_env_tt
  end
class ['a, 'l, 'syn] foldr_llist_t :
  object
    inherit ['a, 'syn, 'syn, 'l, 'syn, 'syn, 'syn, 'syn] llist_tt
    inherit ['a, 'l, 'syn] foldr_llist_env_tt
  end

(** Abstract nat type *)
type 'a lnat =
    O
  | S of 'a
class type virtual ['a, 'ia, 'sa, 'inh, 'syn] lnat_tt =
  object
    method c_O :
      'inh -> ('inh, 'a lnat, 'syn, < a : 'ia -> 'a -> 'sa >) GT.a -> 'syn
    method c_S :
      'inh -> ('inh, 'a lnat, 'syn, < a : 'ia -> 'a -> 'sa >) GT.a ->
        ('ia, 'a, 'sa, < a : 'ia -> 'a -> 'sa >) GT.a -> 'syn
    method t_lnat : ('ia -> 'a -> 'sa) -> 'inh -> 'a lnat -> 'syn
  end
val lnat :
  (('ia -> 'a -> 'sa) -> ('a, 'ia, 'sa, 'inh, 'syn) #lnat_tt -> 'inh ->
    'a lnat -> 'syn, < show : ('a -> string) -> 'a lnat -> string;
  html : ('a -> HTML.viewer) -> 'a lnat -> HTML.viewer;
  eq : ('a -> 'a -> bool) -> 'a lnat -> 'a lnat -> bool;
  compare :
    ('a -> 'a -> GT.comparison) -> 'a lnat -> 'a lnat -> GT.comparison;
  foldl : ('syn -> 'a -> 'syn) -> 'syn -> 'a lnat -> 'syn;
  foldr : ('syn -> 'a -> 'syn) -> 'syn -> 'a lnat -> 'syn;
  gmap : ('a -> 'sa) -> 'a lnat -> 'sa lnat >)
    GT.t
class virtual ['a, 'ia, 'sa, 'inh, 'syn] lnat_t :
  object
    method virtual c_O :
      'inh -> ('inh, 'a lnat, 'syn, < a : 'ia -> 'a -> 'sa >) GT.a -> 'syn
    method virtual c_S :
      'inh -> ('inh, 'a lnat, 'syn, < a : 'ia -> 'a -> 'sa >) GT.a ->
        ('ia, 'a, 'sa, < a : 'ia -> 'a -> 'sa >) GT.a -> 'syn
    method t_lnat : ('ia -> 'a -> 'sa) -> 'inh -> 'a lnat -> 'syn
  end
class type ['a] show_lnat_env_tt = object  end
class type ['a] html_lnat_env_tt = object  end
class type ['a] eq_lnat_env_tt = object  end
class type ['a] compare_lnat_env_tt = object  end
class type ['a, 'syn] foldl_lnat_env_tt = object  end
class type ['a, 'syn] foldr_lnat_env_tt = object  end
class type ['a, 'sa] gmap_lnat_env_tt = object  end
class ['a] show_proto_lnat :
  'a show_lnat_env_tt ref ->
    object inherit ['a, unit, string, unit, string] lnat_tt end
class ['a] html_proto_lnat :
  'a html_lnat_env_tt ref ->
    object
      inherit ['a, unit, HTML.viewer, unit, HTML.viewer] lnat_tt
      method attribute : 'a lnat -> string
      method cname : string -> string
    end
class ['a] eq_proto_lnat :
  'a eq_lnat_env_tt ref ->
    object inherit ['a, 'a, bool, 'a lnat, bool] lnat_tt end
class ['a] compare_proto_lnat :
  'a compare_lnat_env_tt ref ->
    object inherit ['a, 'a, GT.comparison, 'a lnat, GT.comparison] lnat_tt end
class ['a, 'syn] foldl_proto_lnat :
  ('a, 'syn) foldl_lnat_env_tt ref ->
    object inherit ['a, 'syn, 'syn, 'syn, 'syn] lnat_tt end
class ['a, 'syn] foldr_proto_lnat :
  ('a, 'syn) foldr_lnat_env_tt ref ->
    object inherit ['a, 'syn, 'syn, 'syn, 'syn] lnat_tt end
class ['a, 'sa] gmap_proto_lnat :
  ('a, 'sa) gmap_lnat_env_tt ref ->
    object inherit ['a, unit, 'sa, unit, 'sa lnat] lnat_tt end
class ['a] show_lnat_t :
  object
    inherit ['a, unit, string, unit, string] lnat_tt
    inherit ['a] show_lnat_env_tt
  end
class ['a] html_lnat_t :
  object
    inherit ['a, unit, HTML.viewer, unit, HTML.viewer] lnat_tt
    inherit ['a] html_lnat_env_tt
    method attribute : 'a lnat -> string
    method cname : string -> string
  end
class ['a] eq_lnat_t :
  object
    inherit ['a, 'a, bool, 'a lnat, bool] lnat_tt
    inherit ['a] eq_lnat_env_tt
  end
class ['a] compare_lnat_t :
  object
    inherit ['a, 'a, GT.comparison, 'a lnat, GT.comparison] lnat_tt
    inherit ['a] compare_lnat_env_tt
  end
class ['a, 'syn] foldl_lnat_t :
  object
    inherit ['a, 'syn, 'syn, 'syn, 'syn] lnat_tt
    inherit ['a, 'syn] foldl_lnat_env_tt
  end
class ['a, 'syn] foldr_lnat_t :
  object
    inherit ['a, 'syn, 'syn, 'syn, 'syn] lnat_tt
    inherit ['a, 'syn] foldr_lnat_env_tt
  end
class ['a, 'sa] gmap_lnat_t :
  object
    inherit ['a, unit, 'sa, unit, 'sa lnat] lnat_tt
    inherit ['a, 'sa] gmap_lnat_env_tt
  end

(** {3 Library itself } *)

module Bool :
  sig
    type 'a logic' = 'a logic
    type ground = bool
    type logic = bool logic'
    val ground :
      (unit, < compare : ground -> ground -> GT.comparison; eq : ground -> ground -> bool;
      foldl : 'a -> ground -> 'a; foldr : 'a -> ground -> 'a;
      gmap : ground -> ground; html : ground -> HTML.viewer;
      show : ground -> string >)
        GT.t
    val logic :
      (unit, < compare : logic -> logic -> GT.comparison; eq : logic -> logic -> bool;
      foldl : 'a -> logic -> 'a; foldr : 'a -> logic -> 'a;
      gmap : logic -> logic; html : logic -> HTML.viewer;
      show : logic -> string >)
        GT.t
    type groundf = (ground, logic) fancy
    val false_ : groundf
    val true_ : groundf
    val (|^) : groundf -> groundf -> groundf -> goal
    val noto' : groundf -> groundf -> goal
    val noto : groundf -> goal
    val oro : groundf -> groundf -> groundf -> goal
    val (||) : groundf -> groundf -> goal
    val ando : groundf -> groundf -> groundf -> goal
    val (&&) : groundf -> groundf -> goal
    val show_ground : bool -> string
    val inj : bool -> groundf
  end

(** Equality as boolean relation *)
val eqo : ('a, 'b) fancy -> ('a, 'b) fancy -> Bool.groundf -> goal

(** Disequality as boolean relation *)
val neqo : ('a, 'b) fancy -> ('a, 'b) fancy -> Bool.groundf -> goal


module Nat :
  sig
    type 'a logic' = 'a logic
    type 'a t = 'a lnat
    type ground = ground t
    type logic = logic t logic'
    val reifier : var_checker -> (ground, logic) fancy -> logic
    val ground :
      (unit, < compare : ground -> ground -> GT.comparison; eq : ground -> ground -> bool;
      foldl : 'a -> ground -> 'a; foldr : 'a -> ground -> 'a;
      gmap : ground -> ground; html : ground -> HTML.viewer;
      show : ground -> string >)
        GT.t
    val logic :
      (unit, < compare : logic -> logic -> GT.comparison; eq : logic -> logic -> bool;
      foldl : 'a -> logic -> 'a; foldr : 'a -> logic -> 'a;
      gmap : logic -> logic; html : logic -> HTML.viewer;
      show : logic -> string >)
        GT.t
    val of_int : int -> ground
    val to_int : ground -> int
    type groundf = (ground, logic) fancy
    val addo : groundf -> groundf -> groundf -> goal
    val (+) : groundf -> groundf -> groundf -> goal
    val mulo : groundf -> groundf -> groundf -> goal
    val ( * ) : groundf -> groundf -> groundf -> goal
    val leo : groundf -> groundf -> Bool.groundf -> goal
    val geo : groundf -> groundf -> Bool.groundf -> goal
    val gto : groundf -> groundf -> Bool.groundf -> goal
    val lto : groundf -> groundf -> Bool.groundf -> goal
    val (<=) : groundf -> groundf -> goal
    val (>=) : groundf -> groundf -> goal
    val (>) : groundf -> groundf -> goal
    val (<) : groundf -> groundf -> goal
    val show_ground : ground -> string
  end

(** [inj_nat n] is a deforested synonym for injection *)
val inj_nat : int -> (Nat.ground, Nat.logic) fancy


module List :
  sig
    include module type of struct include List end
    type 'a logic' = 'a logic
    type ('a, 'l) t = ('a, 'l) llist
    type 'a ground = ('a, 'a ground) t
    type 'a logic = ('a, 'a logic) t logic'
    val ground :
      (unit, < gmap : ('a -> 'b) -> 'a ground -> 'b ground;
      compare :
        ('a -> 'a -> GT.comparison) -> 'a ground -> 'a ground ->
          GT.comparison;
      eq : ('a -> 'a -> bool) -> 'a ground -> 'a ground -> bool;
      foldl : ('b -> 'a -> 'b) -> 'b -> 'a ground -> 'b;
      foldr : ('b -> 'a -> 'b) -> 'b -> 'a ground -> 'b;
      html : ('a -> HTML.viewer) -> 'a ground -> HTML.viewer;
      show : ('a -> string) -> 'a ground -> string >)
        GT.t
    val of_list : ('a, 'b) fancy list -> ('a ground, 'b logic) fancy
    val logic :
      (unit, < gmap : ('a -> 'b) -> (('a, 'c) t logic' as 'c) -> (('b, 'd) t logic' as 'd);
      compare :
        ('a -> 'a -> GT.comparison) -> 'a logic -> 'a logic -> GT.comparison;
      eq : ('a -> 'a -> bool) -> 'a logic -> 'a logic -> bool;
      foldr : ('b -> 'a -> 'b) -> 'b -> 'a logic -> 'b;
      foldl : ('b -> 'a -> 'b) -> 'b -> 'a logic -> 'b;
      html : ('a -> HTML.viewer) -> 'a logic -> HTML.viewer;
      show : ('a -> string) -> 'a logic -> GT.string >)
        GT.t
    type ('a, 'b) flist = ('a ground, 'b logic) fancy
    val flist :
      (unit, < show : ('a -> string) -> ('a, _) flist -> GT.string >) GT.t
    val reifier :
      (var_checker -> ('a, 'b) fancy -> 'b) -> var_checker ->
        ('a ground, 'b logic) fancy -> 'b logic
    val cons : ('a, 'b) fancy -> ('a, 'b) flist -> ('a, 'b) flist
    val foldro :
      (('a, 'b) fancy -> ('acc, 'acc2) fancy -> ('acc, 'acc2) fancy ->
         goal) ->
        ('acc, 'acc2) fancy -> ('a, 'b) flist -> ('acc, 'acc2) fancy -> goal
    val mapo :
      (('a, 'b) fancy -> ('q, 'w) fancy -> goal) -> ('a, 'b) flist ->
        ('q, 'w) flist -> goal
    val filtero :
      (('a, 'b) fancy -> Bool.groundf -> goal) -> ('a, 'b) flist ->
        ('a, 'b) flist -> goal
    val lookupo :
      (('a, 'b) fancy -> Bool.groundf -> goal) -> ('a, 'b) flist ->
        ('a option, 'b option logic) fancy -> goal
    val anyo : (Bool.ground, Bool.logic) flist -> Bool.groundf -> goal
    val allo : (Bool.ground, Bool.logic) flist -> Bool.groundf -> goal
    val lengtho : (_, _) flist -> Nat.groundf -> goal
    val appendo : ('a, 'b) flist -> ('a, 'b) flist -> ('a, 'b) flist -> goal
    val reverso : ('a, 'b) flist -> ('a, 'b) flist -> goal
    val membero : ('a, 'b) flist -> ('a, 'b) fancy -> goal
    val nullo : _ flist -> goal
    val caro : ('a, 'b) flist -> ('a, 'b) fancy -> goal
    val hdo : ('a, 'b) flist -> ('a, 'b) fancy -> goal
    val cdro : ('a, 'b) flist -> ('a, 'b) flist -> goal
    val tlo : ('a, 'b) flist -> ('a, 'b) flist -> goal
  end

(** [inj_list l] is a deforested synonym for injection *)
val inj_list : ('a, 'b) fancy list -> ('a, 'b) List.flist

val inj_pair :
  ('a, 'b) fancy -> ('c, 'd) fancy -> ('a * 'c, ('b * 'd) logic) fancy
val inj_list_p :
  (('a, 'b) fancy * ('c, 'd) fancy) list ->
    ('a * 'c, ('b * 'd) logic) List.flist
val inj_int : int -> (int, int logic) fancy

(** [inj_nat_list l] is a deforsted synonym for injection *)
val inj_nat_list : int list -> (Nat.ground, Nat.logic) List.flist

(** Infix synonym for [Cons] *)
val (%) : ('a, 'b) fancy -> ('a, 'b) List.flist -> ('a, 'b) List.flist

(** [x %< y] is a synonym for [Cons (x, !(Cons (y, !Nil)))] *)
val (%<) : ('a, 'b) fancy -> ('a, 'b) fancy -> ('a, 'b) List.flist

(** [!< x] is a synonym for [Cons (x, !Nil)] *)
val (!<) : ('a, 'b) fancy -> ('a, 'b) List.flist

(** [nil] is a synonym for [inj Nil] *)
val nil : unit -> (_, _) List.flist
