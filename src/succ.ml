
module X  : sig
  type 'a logic
  type goal
  type 'a a_la_goal
  type sss

  val call_fresh: ('a logic -> goal) -> goal
  
  module LogicAdder : sig
    val zero: 'a -> 'a
    val succ: ('a -> 'b a_la_goal) -> ('c logic -> 'a) -> ('c logic * 'b) a_la_goal
  end

  val demo0: sss a_la_goal
  val demo1: int logic -> sss a_la_goal
  val demo2: int logic -> string logic -> sss a_la_goal

  val run: ('a -> 'b) -> 'a -> 'b
end = struct
  type 'a logic = int
  type state = string
  type 'a a_la_goal = state -> 'a
  type sss = state Stream.t
  type goal = sss a_la_goal

  let call_fresh: ('a logic -> state -> 'r) -> state -> 'r = fun f st -> f 5 st

  let demo0: goal = fun _ -> Stream.of_list []
  let demo1: int logic -> goal = fun _ -> demo0
  let demo2: int logic -> string logic -> goal = fun _ _ -> demo0

  module LogicAdder = struct
    (* let zero: ('a -> state -> 'b) -> ('a -> state -> 'b) = fun f -> f *)
    let zero = fun f -> f

    let succ prev f =
      call_fresh (fun logic st -> (logic, prev (f logic) st))

    let (_:int) = succ zero demo1

  end


  let run num g =
    num g
end

let (_:int) = X.run X.LogicAdder.(succ @@ succ zero) X.demo2
