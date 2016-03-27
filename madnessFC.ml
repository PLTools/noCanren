let id x = x;;
let option_bind f = function Some x -> f x | None -> None

module type MAP_INTERFACE = sig
  type 'a logic = Var of int | Value of 'a
  type state
  val empty_state: unit -> state
  val find_value : 'a logic -> state -> 'a logic option
end
module Map () : MAP_INTERFACE = struct
  (* map which is used to represent state *)

  type 'a logic = Var of int | Value of 'a

  type state = { last: int; foo: int -> Obj.t option }
  let empty_state () = { last=10; foo= fun _ -> None }

  (* for Var _ -return Value _ when first variable has substitution in the current state,
     if not returns this variable  *)
  let find_value : 'a logic -> state -> 'a logic option=
    fun q st ->
      match q with
      | Var n -> st.foo n |> Obj.magic
      | Value _ -> Some q

  let make_fresh: state -> 'a logic * state = fun st ->
    (Var st.last, { st with last = st.last+1 } )
end

module type STATE_WITH_OPTS = sig
  module X : MAP_INTERFACE
  val state : X.state
end

let run : ((module STATE_WITH_OPTS) -> 'a) -> 'a = fun f ->
  let module M = struct
     module X : MAP_INTERFACE = Map ()
     let state = X.empty_state ()
  end in
  f (module M: STATE_WITH_OPTS)


let call_fresh: ('a logic (* from STATE_WITH_OPTS *) -> (module STATE_WITH_OPTS) -> 'rez) -> (module STATE_WITH_OPTS) -> 'rez =
  fun f st -> failwith "how to implement this"
