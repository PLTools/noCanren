type 'a maybe

val fail : 'a maybe
val return : 'a -> 'a maybe
val ( let* ) : 'a maybe -> ('a -> 'b maybe) -> 'b maybe
val ( <&&> ) : bool maybe -> bool maybe -> bool maybe
val ( <||> ) : bool maybe -> bool maybe -> bool maybe
val from_just : 'a maybe -> 'a

open OCanren

module HO : sig
  val fail_o : 'a ilogic -> goal
  val return_o : ('a ilogic -> goal) -> 'a ilogic -> goal

  val let_star_bind_o
    :  ('a ilogic -> goal)
    -> (('a ilogic -> goal) -> 'b ilogic -> goal)
    -> 'b ilogic
    -> goal

  val c60c38c38c62_o
    :  (bool ilogic -> goal)
    -> (bool ilogic -> goal)
    -> bool ilogic
    -> goal

  val c60c124c124c62_o
    :  (bool ilogic -> goal)
    -> (bool ilogic -> goal)
    -> bool ilogic
    -> goal

  val from_just_o : ('a ilogic -> goal) -> 'a ilogic -> goal
end

module FO : sig
  val fail : 'a ilogic -> goal
  val return : 'a ilogic -> 'a ilogic -> goal
  val let_star_bind : 'a ilogic -> ('a ilogic -> 'b ilogic -> goal) -> 'b ilogic -> goal
  val c60c38c38c62 : bool ilogic -> bool ilogic -> bool ilogic -> goal
  val c60c124c124c62 : bool ilogic -> bool ilogic -> bool ilogic -> goal
  val from_just : 'a ilogic -> 'a ilogic -> goal
end