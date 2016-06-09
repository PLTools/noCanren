open Printf

let make_buffer () = Buffer.create 20
let bprintf b = Printf.ksprintf (Buffer.add_string b)

module type SHOW = sig
    type t
    val show : t -> string
end

let show {S : SHOW} x = S.show x;;

implicit module Show_float: (SHOW with type t = float) = struct
    type t = float
    let show x = sprintf "%f" x
end

module Show_list_explicit {X : SHOW}: (SHOW with type t=X.t list) = struct
    type t = X.t list
    let show xs =
      let b = Buffer.create 30 in
      bprintf b "Show_list: [";
      List.iter (fun x -> bprintf b "%s" @@ X.show x) xs;
      bprintf b "]%!";
      Buffer.contents b
end

implicit module Show_list = Show_list_explicit

module Show_option_explicit {X : SHOW}: (SHOW with type t=X.t option) = struct
    type t = X.t option
    let show = function
      | None -> "None"
      | Some x -> sprintf "Some (%s)" (X.show x)
end

implicit module Show_option = Show_option_explicit

implicit module Show_int : (SHOW with type t = int) = struct
    type t = int
    let show x = sprintf "%d" x
end

implicit module Show_bool : (SHOW with type t = bool) = struct
    type t = bool
    let show x = sprintf "%b" x
end

implicit module Show_string : (SHOW with type t = string) = struct
    type t = string
    let show x = x
end

implicit module Show_pair {X : SHOW} {Y : SHOW}: (SHOW with type t = X.t * Y.t) = struct
    type t = X.t * Y.t
    let show (x,y) = sprintf "(%s,%s)" (X.show x) (Y.show y)
end

module Show_pair_explicit (X : SHOW) (Y : SHOW): (SHOW with type t = X.t * Y.t) = struct
    type t = X.t * Y.t
    let show (x,y) = sprintf "(%s,%s)" (X.show x) (Y.show y)
end


implicit module Show_tiplet {X : SHOW} {Y : SHOW} {Z : SHOW}:
   (SHOW with type t = X.t * Y.t * Z.t) =
struct
    type t = X.t * Y.t * Z.t
    let show (x,y,z) = sprintf "(%s,%s,%s)" (X.show x) (Y.show y) (Z.show z)
end

module Show_tiplet_explicit (X : SHOW) (Y : SHOW) (Z : SHOW):
   (SHOW with type t = X.t * Y.t * Z.t) =
struct
    type t = X.t * Y.t * Z.t
    let show (x,y,z) = sprintf "(%s,%s,%s)" (X.show x) (Y.show y) (Z.show z)
end

(*
(****************************************************)
type 'a logic = Var of int * 'a logic list | Value of 'a * ('a -> string)
implicit module Show_logic {X : SHOW} =
struct
  type t = X.t logic
  let show l = "asdf"
end

type 'a llist = Nil | Cons of 'a logic * 'a llist logic
implicit module Show_llist {X: SHOW} = struct
  type t = X.t llist
  let show _ = "llist"
end

let (!) {S : SHOW} x = Value (x, S.show)
type goal
let call_fresh : ('a logic -> goal) -> goal = fun _ -> Obj.magic ()
let (===) : 'a logic -> 'a logic -> goal =  fun _ _ -> Obj.magic ()
let (&&&) : goal -> goal -> goal =  fun x _ -> x
let (%) : 'a logic -> 'a llist logic -> 'a llist logic = fun _ _ -> Obj.magic Nil

let rec wtf (ps: int llist logic)
    (ans: (int logic * int list option) logic) =
  call_fresh (fun h -> call_fresh (fun tl ->
      (ps === h%tl) &&&
      (ans === !(h, (None: int list option)) )
    ))
  *)
