let id x = x;;

(* module PolyVar: sig *)
(*   val zero : ('a list -> 'b) -> 'b *)
(*   val s : (('a list -> 'b) -> 'c) -> ('a list -> 'b) -> 'a -> 'c *)
(*   val p : (('a -> 'a) -> 'b) -> 'b *)
(* end = struct *)
(*   let zero = fun k -> k [] *)
(*   let s prev k = fun x -> prev (fun v -> k (x::v)) *)
(*   let p sel = sel id *)

(*   let one = s zero *)
(*   let two = s one *)
(* end;; *)

type 'a logic = Var of int | Value of 'a

(* State also contains some information about logic variables' bindings but is is omitted for simplicity *)
type state = { mutable last: int }

(* for Var _ -return Value _ when first variable has substitution in the current state,
   if not returns this variable  *)
let find_value : 'a . 'a logic -> state -> 'a logic = fun _ _ -> Obj.magic ()

let empty_state () = { last=1 }

type goal = state -> state list

let call_fresh : ('a logic -> state -> 'rez) -> state -> 'rez = fun f state ->
  let v = Var state.last in
  let new_state = { last = state.last+1} in
  f v new_state
;;

(* one variable is equal (unified) with another *)
let (===) : 'a logic -> 'a logic -> goal = fun _a _b -> fun _ -> [];;
(* conjunction *)
let (&&&) : goal -> goal -> goal = Obj.magic ()

let demo1 a = (a === (Value 5));;
(* - : int logic -> goal = <fun> *)

let demo2 n s = (n === (Value 5)) &&& (s === (Value "aadf"));;

module VariadicWTF = struct
  (* don't know how to call it right *)
  let zero f = f

  let succ (prev: 'a -> state -> 'z) (f: 'b logic -> 'a) : state -> 'z =
    call_fresh (fun logic -> prev @@ f logic)
end

(*  VariadicWTF module allows to write some weird stuff like
 *    let _: state list = run (succ zero)         (fun q   -> a)
 *    let _: staet list = run (succ @@ succ zero) (fun q r -> a)
 *)



let run : (state -> 'a) -> 'a = fun f -> f @@ empty_state ()

let run_attempt1 runner goal =
  let st = empty_state () in
  runner goal st

let (_: ('a logic ->             state -> state list) -> state list) = run_attempt1 VariadicWTF.(succ zero)
let (_: ('a logic -> 'b logic -> state -> state list) -> state list) = run_attempt1 VariadicWTF.(succ @@ succ zero)

(* last two lines demonstrate that it seems to work when we have function `find_value`. But the problem is that state is exposed
 * If variable 1 has binding to int value in the state1 and variable 2 has binding to string in state 2 we can call
   `find_value variable1 state2`  and `find_value variable2 state1` which is wrong because we will get value with bad type.
 *)

(* the idea to fix problem above is not allow user to get variable from the state where this variable was not created *)

(* The PolyPairsNaive module allows to  write functions which convert arguments to pairs
 * # PolyPairsNaive.(p  one);;
 * - : '_a -> '_a * unit = <fun>
 * # PolyPairsNaive.(p (s one));;
 * - : '_a -> '_b -> '_a * ('_b * unit) = <fun>
 *)
module PolyPairsNaive :
  sig
    val one : ('a * unit -> 'b) -> 'a -> 'b
    val succ : (('a -> 'b) -> 'c) -> ('d * 'a -> 'b) -> 'd -> 'c
    val p : (('a -> 'a) -> 'b) -> 'b
  end
= struct
  let one = fun k x -> k (x,())
  let succ = fun prev k x -> prev (fun v -> k (x,v))
  let p sel = sel (fun x -> x)

  (* we don't add definitions of two and three because of apeearence of value restriction *)
  (* maybe it is possible to fix but it is another problem *)
end;;

(* So, the idea is to rewrite 'run_attempt1' using PolyPairs. The expected use case will be
 *   let var1info,()              = run VariadicWTF.one        PolyPairsNaive.one        (fun q   -> demo1 q)
 *   let (var2info,(var1info,())) = run VariadicWTF.(succ one) PolyPairsNaive.(succ one) (fun q r -> demo2 q r)
 *
 *)

let run_attempt2 wtf_num pair_num goal () =
  let part = wtf_num goal in
  let _vars = pair_num part in

  let st = empty_state () in
  let _states = part st in
  (* What should we 'return' from the function? We have list of states where values can be searched and tuple with vriables *)
  ()

let (_: unit -> unit) = run_attempt2 VariadicWTF.(        succ zero)  PolyPairsNaive.(p @@ one)     (fun q   -> demo1 q)
let (_: unit -> unit) = run_attempt2 VariadicWTF.(succ @@ succ zero)  PolyPairsNaive.(p (succ one)) (fun q r -> demo2 q r)





(* PROBLEMS BEGAN BELOW *)
(* To write run_attempt 3 we are going to rewrite again PolyPairs, they will take additional function, which
 * takes state list and logic variable and return list of values of this logic variable in the specified state  *)


module PolyPairs = struct
  let one: ( 'a list * unit -> 'b) -> ('a -> 'a list) -> 'a -> 'b = fun k mapper x -> k ( (mapper x), ())
  let succ = fun prev k (mapper: 'z -> 'z list) x -> prev (fun v -> k (mapper x,v)) mapper
  let p sel = sel (fun x -> x)
end;;

module PolyPairs2 = struct
  let one: ( (state -> 'a ) * unit -> 'b) -> 'a -> 'b = fun k x -> k ( find_value x, ())
  let succ = fun prev k (mapper: 'z -> 'z list) x -> prev (fun v -> k (find_value x,v))
  let p sel = sel (fun x -> x)
end;;

(*
# PolyPairs2.(succ one id);;
- : ('_a -> '_a list) ->
    '_b logic ->
    '_c logic -> (state -> '_b logic) * ((state -> '_c logic) * unit)
= <fun>
*)

module PolyPairs3 = struct
  let mapper st ls = List.map (fun var -> find_value var st) ls

  let one: ( 'a list * unit -> 'b) -> state -> 'a list -> 'a -> 'b = fun k st ls x -> k (mapper st ls, ())
  let succ = fun prev k st ls -> prev (fun v -> k (mapper st ls,v)) st ls
  let p sel = sel (fun x -> x)
end;;

module PolyPairs4 :
  sig
    val mapper : 'a logic -> state list -> 'a logic list
    val one : ('a logic list * unit -> 'b) -> state list -> 'a logic -> 'b
    val succ :
      (('a -> 'b) -> state list -> 'c) ->
      ('d logic list * 'a -> 'b) -> state list -> 'd logic -> 'c
    val p : (('a -> 'a) -> 'b) -> 'b
  end
= struct
  let mapper var sts = List.map (find_value var) sts

  let one: ( 'a list * unit -> 'b) -> state list -> 'a -> 'b = fun k sts var -> k (mapper var sts, ())
  let succ = fun prev k sts var -> prev (fun v -> k (mapper var sts,v)) sts
  let p sel = sel (fun x -> x)
end;;

(*
# PolyPairs4.(succ (succ one) id);;
- : state list ->
    '_a logic ->
    '_b logic ->
    '_c logic -> '_a logic list * ('_b logic list * ('_c logic list * unit))
= <fun>
*)

(*  The expected use case of this was something like this: *)

let run_attempt3 wtf_num pair_num goal () =
  let part = wtf_num goal in

  let st = empty_state () in
  let states: state list = part st in
  pair_num (fun q -> List.map (find_value q) states) goal

  (* What should we 'return' from the function? We have list of states where values can be searched and tuple with vriables *)
  (* All this stuff with right application of pair_num is tricky *)


let run_attempt4 wtf_num pair_num goal () =
  let part = wtf_num goal in

  object
    method part = part
    method num1 = wtf_num
    method num2 = pair_num
  end
  (* let st = empty_state () in *)
  (* let states: state list = part st in *)
  (* pair_num (fun q -> List.map (find_value q) states) goal *)


let list_take ~n xs =
  let rec helper acc n = function
  | _ when n = 0 -> List.rev acc
  | [] -> failwith "bad argument of list_take"
  | x::xs -> helper (x::acc) (n-1) xs
  in
  helper [] n xs
;;
(* Let's try to combine VariadicWTF and PolyPairs into one *)
module VariadicWTF2 = struct
  (* don't know how to call it right *)
  (* let zero f = f *)

  let one f =
   call_fresh (fun logic -> (fun st -> let r = f logic st in (r, List.map (find_value logic) r) ) )
  (*
  val one :
      ('a logic -> state -> state list) ->
      state -> state list * 'a logic list
  *)

 let mapper logic r = fun ~n -> list_take ~n @@ List.map (find_value logic) r
 let one f =
   call_fresh (fun logic -> (fun st -> let r = f logic st in (mapper logic r) ) )
  (* val one : *)
  (*   ('a logic -> state -> state list) -> state -> n:int -> 'a logic list *)

 let one f =
   call_fresh (fun logic -> (fun st -> let r = f st logic  in (mapper logic r, ()) ) )

  let succ prev f =
    call_fresh (fun logic -> (fun st -> mapper logic (f st logic), prev (fun st -> f st logic) st (* logic @@ (f logic) *) ) )
end


(*
 * # PolyPairs.(p one);;
 * - : ('_a -> '_a list) -> '_a -> '_a list * unit = <fun>
 * # PolyPairs.(p (succ one));;
 * - : ('_a -> '_a list) -> '_a -> '_a -> '_a list * ('_a list * unit) = <fun>
 *
 * # PolyPairsNaive.(p one);;
 * - : '_a -> '_a * unit = <fun>
 * # PolyPairsNaive.(p (succ one));;
 * - : '_a -> '_b -> '_a * ('_b * unit) = <fun>
 *
 * WE LOST POLYMOPRPHISM WTF!!!
 *)


module PolyPairs5
= struct
  let mapper var sts = List.map (find_value var) sts

  let one: ( (state list -> 'a list) * unit -> 'b) -> 'a -> 'b = fun k var -> k (mapper var, ())
  let succ = fun prev k var -> prev (fun v -> k (mapper var,v))
  let p sel = sel (fun x -> x)
end;;


let run_attempt5 wtf_num goal () =
  let part = wtf_num goal in

  let st = empty_state () in
  let (states,r): (state list * _)  = part st in
  (* pair_num (fun q -> List.map (find_value q) states) goal *)
  (states,r)



module PolyPairs6
= struct
  let mapper var sts = List.map (find_value var) sts

  type stream = state list
  let one: ( 'a list * unit -> 'b) -> stream -> 'a -> 'b = fun k stream var -> k (mapper var stream, ())
  let succ = fun prev k -> fun stream var -> prev (fun v -> k (mapper var stream,v)) stream
  let p sel = sel (fun x -> x)
end;;
