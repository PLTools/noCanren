open Printf

module Stream =
struct

  type 'a t = Nil | Cons of 'a * 'a t | Lazy of 'a t Lazy.t

  let from_fun (f: unit -> 'a t) : 'a t = Lazy (Lazy.from_fun f)

  let nil = Nil

  let cons h t = Cons (h, t)

  let rec take ?(n=(-1)) s =
    if n = 0
    then []
    else match s with
      | Nil          -> []
      | Cons (x, xs) -> x :: take ~n:(n-1) xs
      | Lazy  z      -> take ~n:n (Lazy.force z)

  let rec mplus fs gs =
    (* LOG[trace1] (logn "mplus"); *)
    from_fun (fun () ->
        match fs with
        | Nil           -> gs
        | Cons (hd, tl) -> cons hd (mplus gs tl)
        | Lazy z        -> mplus gs (Lazy.force z)
      )

  let rec bind xs f =
    from_fun (fun () ->
        match xs with
        | Cons (x, xs) -> mplus (f x) (bind xs f)
        | Nil          -> nil
        | Lazy z       -> bind (Lazy.force z) f
      )

  let rec map f xs =
    from_fun (fun () ->
      match xs with
      | Nil -> Nil
      | Lazy z -> map f (Lazy.force z)
      | Cons (x,xs) -> cons (f x) (map f xs)
    )

  let rec concat xs ys =
    from_fun (fun () ->
      match xs with
      | Nil -> ys
      | Lazy z -> concat (Lazy.force z) ys
      | Cons (x,xs) -> cons x (concat xs ys)
    )
end


module type LOGGER = sig
  type t
  type node
  val create: unit -> t
  val make_node: t -> node
  val connect: t -> node -> node -> string -> unit
  val output_plain: filename:string -> t -> unit
  val output_html : filename:string -> string list -> t -> unit
end

module UnitLogger : LOGGER = struct
  type t = unit
  type node = unit
  let create () = ()
  let make_node () = ()
  let connect _ _ _ _ = ()
  let output_plain ~filename () = ()
  let output_html  ~filename _ () = ()
end

(*
let (!!) = Obj.magic

type var = Var of int
type w   = Unboxed of Obj.t | Boxed of int * int * (int -> Obj.t) | Invalid of int

type config =
  { mutable do_log: bool;
    mutable do_readline: bool }

let config = { do_log=true; do_readline=false }

let () =
  let args = ref [("-r", Arg.Unit (fun () -> config.do_readline <- true), "readlines")] in
  LOG[trace1](
    args := ("-q", Arg.Unit (fun () -> config.do_log <- false), "quite") :: !args;
    args := ("-v", Arg.Unit (fun () -> config.do_log <- true), "verbose") :: !args );
  Arg.parse !args
    (fun s -> Printf.eprintf "Unknown parameter '%s'\n" s; exit 0)
    "This is usage message"

let logn fmt =
  if config.do_log then Printf.kprintf (Printf.printf "%s\n%!") fmt
  else Printf.kprintf (fun fmt -> ignore (Printf.sprintf "%s" fmt)) fmt

let logf fmt =
  if config.do_log then Printf.kprintf (Printf.printf "%s%!") fmt
  else Printf.kprintf (fun fmt -> ignore (Printf.sprintf "%s" fmt)) fmt
*)

let (!!) = Obj.magic

type 'a logic = Var of int | Value of 'a * ('a -> string)

let (!) x = Value (x, (fun _ -> "<not implemented>"))
let embed {S : ImplicitPrinters.SHOW} x = Value (x, S.show)

module Show_logic_impl {X : ImplicitPrinters.SHOW} = struct
    type t = X.t logic
    let show l =
      match l with
      | Var n -> sprintf "_.%d" n
      | Value (x,_) -> X.show x
end

implicit module Show_logic = Show_logic_impl

let show_logic_naive =
  function
  | Var n -> sprintf "_.%d" n
  | Value (x,printer) -> printer x

(* let logic = { *)
(*   logic with plugins = *)
(*     object *)
(*       method html    = logic.plugins#html *)
(*       method eq      = logic.plugins#eq *)
(*       method compare = logic.plugins#compare *)
(*       method foldr   = logic.plugins#foldr *)
(*       method foldl   = logic.plugins#foldl *)
(*       method map     = logic.plugins#map *)
(*       method show fa x = *)
(*         GT.transform(logic) *)
(*            (GT.lift fa) *)
(*            (object inherit ['a] @logic[show] *)
(*               method c_Var   _ _ i = Printf.sprintf "_.%d" i *)
(*               method c_Value _ _ x = x.GT.fx () *)
(*             end) *)
(*            () *)
(*            x *)
(*     end *)
(* };; *)

type 'a llist = Nil | Cons of 'a logic * 'a llist logic

let llist_nil = Value(Nil, fun _ -> "[]")
let llist_printer v =
  let b = Buffer.create 49 in
  let rec helper = function
    | Cons (h,tl) -> begin
        Buffer.add_string b (show_logic_naive h);
        Buffer.add_string b " :: ";
        match tl with
        | Var n -> Buffer.add_string b (show_logic_naive tl)
        | Value (v,pr) -> Buffer.add_string b (pr v)
      end
    | Nil -> Buffer.add_string b "[]"
  in
  helper v;
  Buffer.contents b

let (%)  x y =
  Value (Cons (x, y), llist_printer)

let (%<) x y =
  let ans = Value( Cons(y,llist_nil), llist_printer) in
  let ans = Value( Cons(x,ans), llist_printer) in
  ans

let (!<) x   =
  let printer = function
    | Cons (v, Value (Nil,_)) -> sprintf "[%s]" (show_logic_naive v)
    | _ -> assert false
  in
  Value (Cons (x, !Nil), printer)

let of_list {S : ImplicitPrinters.SHOW} xs =
  let rec helper = function
    | [] -> llist_nil
    | x::xs -> (Value (x, S.show)) % (helper xs)
  in
  helper xs

exception Not_a_value
exception Occurs_check

let rec to_listk k = function
| Value (Nil,_) -> []
| Value (Cons (Value (x,_), xs),_) -> x :: to_listk k xs
| z -> k z

let to_list l = to_listk (fun _ -> raise Not_a_value) l

(* let llist = { *)
(*   llist with plugins = *)
(*     object *)
(*       method html    = llist.plugins#html *)
(*       method eq      = llist.plugins#eq *)
(*       method compare = llist.plugins#compare *)
(*       method foldr   = llist.plugins#foldr *)
(*       method foldl   = llist.plugins#foldl *)
(*       method map     = llist.plugins#map *)
(*       method show fa x = "[" ^ *)
(*         (GT.transform(llist) *)
(*            (GT.lift fa) *)
(*            (object inherit ['a] @llist[show] *)
(*               method c_Nil   _ _      = "" *)
(*               method c_Cons  i s x xs = GT.show(logic) fa x ^ (match xs with Value Nil -> "" | _ -> "; " ^ GT.show(logic) (s.GT.f i) xs) *)
(*             end) *)
(*            () *)
(*            x *)
(*         ) ^ "]" *)
(*     end *)
(* } *)

type w = Unboxed of Obj.t | Boxed of int * int * (int -> Obj.t) | Invalid of int

let rec wrap (x : Obj.t) =
  Obj.(
    let is_valid_tag =
      List.fold_left
        (fun f t tag -> tag <> t && f tag)
        (fun _ -> true)
        [lazy_tag   ; closure_tag  ; object_tag  ; infix_tag ;
         forward_tag; no_scan_tag  ; abstract_tag; custom_tag;
         custom_tag ; unaligned_tag; out_of_heap_tag
        ]
    in
    let is_unboxed obj =
      is_int obj ||
      (fun t -> t = string_tag || t = double_tag) (tag obj)
    in
    if is_unboxed x
    then Unboxed x
    else
      let t = tag x in
      if is_valid_tag t
      then
        let f = if t = double_array_tag then !! double_field else field in
        Boxed (t, size x, f x)
      else Invalid t
  )

let generic_show x =
  let x = Obj.repr x in
  let b = Buffer.create 1024 in
  let rec inner o =
    match wrap o with
    | Invalid n             -> Buffer.add_string b (Printf.sprintf "<invalid %d>" n)
    | Unboxed n when !!n=0  -> Buffer.add_string b "[]"
    | Unboxed n             -> Buffer.add_string b (Printf.sprintf "int<%d>" (!!n))
    | Boxed (t,l,f) when t=0 && l=1 && (match wrap (f 0) with Unboxed i when !!i >=10 -> true | _ -> false) ->
      Printf.bprintf b "var%d" (match wrap (f 0) with Unboxed i -> !!i | _ -> failwith "shit")

    | Boxed   (t, l, f) ->
      Buffer.add_string b (Printf.sprintf "boxed %d <" t);
      for i = 0 to l - 1 do (inner (f i); if i<l-1 then Buffer.add_string b " ") done;
      Buffer.add_string b ">"
  in
  inner x;
  Buffer.contents b

module Env :
  sig
    type t

    val empty  : unit -> t
    val fresh  : t -> 'a logic * t
    val var    : t -> 'a logic -> int option
    val vars   : t -> unit logic list
    val show   : t -> string
  end =
  struct
    module H = Hashtbl.Make (
      struct
        type t = unit logic
        let hash = Hashtbl.hash
        let equal = (==)
      end)

    type t = unit H.t * int

    let counter_start = 10 (* 1 to be able to detect empty list *)
    let empty () = (H.create 1024, counter_start)

    let fresh (h, current) =
      let v = Var current in
      H.add h v ();
      (!!v, (h, current+1))

    let var (h, _) x =
      if H.mem h (!! x)
      then let Var i = !! x in Some i
      else None

    let vars (h, _) = H.fold (fun v _ acc -> v :: acc) h []

    let show env = (List.fold_left (fun acc (Var i) -> acc ^ (Printf.sprintf "$%d; " i)) "env {" (vars env)) ^ "}"

  end

module Subst :
  sig
    type t

    val empty   : t

    val of_list : (int * Obj.t * Obj.t) list -> t
    val split   : t -> Obj.t list * Obj.t list
    val walk    : Env.t -> 'a logic -> t -> 'a logic
    val walk'   : Env.t -> 'a logic -> t -> 'a logic
    val unify   : Env.t -> 'a logic -> 'a logic -> t option -> (int * Obj.t * Obj.t) list * t option
    val show    : t -> string
  end =
  struct
    module M = Map.Make (struct type t = int let compare = Pervasives.compare end)

    type t = (Obj.t * Obj.t) M.t

    let show m = (M.fold (fun i (_, x) s -> s ^ Printf.sprintf "%d -> %s; " i (generic_show x)) m "subst {") ^ "}"

    let empty = M.empty

    let of_list l = List.fold_left (fun s (i, v, t) -> M.add i (v, t) s) empty l

    let split s = M.fold (fun _ (x, t) (xs, ts) -> x::xs, t::ts) s ([], [])

    let rec walk env var subst =
      match Env.var env var with
      | None   -> var
      | Some i ->
          try walk env (snd (M.find i (!! subst))) subst with Not_found -> var

    let rec occurs env xi term subst =
      let y = walk env term subst in
      match Env.var env y with
      | Some yi -> xi = yi
      | None ->
         let wy = wrap (Obj.repr y) in
	 match wy with
	 | Unboxed _ -> false
	 | Invalid n -> invalid_arg (Printf.sprintf "Invalid value in occurs check (%d)" n)
	 | Boxed (_, s, f) ->
            let rec inner i =
              if i >= s then false
	      else occurs env xi (!!(f i)) subst || inner (i+1)
	    in
	    inner 0

    let rec walk' env var subst =
      match Env.var env var with
      | None ->
          let Value (v, printer) = !! var in
	  (match wrap (Obj.repr v) with
	   | Unboxed _ -> !!var
	   | Invalid n -> invalid_arg (sprintf "Invalid value for reconstruction (%d)" n)
	   | Boxed (t, s, f) ->
               let var = Obj.dup (Obj.repr v) in
               let sf =
		 if t = Obj.double_array_tag
		 then !! Obj.set_double_field
		 else Obj.set_field
	       in
	       for i = 0 to s - 1 do
                 sf var i (!!(walk' env (!!(f i)) subst))
               done;
	       Value(!!var, printer)
          )

      | Some i ->
	  (try walk' env (snd (M.find i (!! subst))) subst
	   with Not_found -> !!var
	  )


    let unify env x y subst =
      let rec unify x y (delta, subst) =
        let extend xi x term delta subst =
          (* if occurs env xi term subst then raise Occurs_check *)
          (* else  *)
            (xi, !!x, !!term)::delta, Some (!! (M.add xi (!!x, term) (!! subst)))
        in
        match subst with
        | None -> delta, None
        | (Some subst) as s ->
            let x, y = walk env x subst, walk env y subst in
            match Env.var env x, Env.var env y with
            | Some xi, Some yi -> if xi = yi then delta, s else extend xi x y delta subst
            | Some xi, _       -> extend xi x y delta subst
	    | _      , Some yi -> extend yi y x delta subst
	    | _ ->
                let Value (xx,xprinter) = !!x in
                let Value (yy,yprinter) = !!y in
	        let wx, wy = wrap (Obj.repr xx), wrap (Obj.repr yy) in
                (match wx, wy with
                 | Unboxed vx, Unboxed vy -> if vx = vy then delta, s else delta, None
                 | Boxed (tx, sx, fx), Boxed (ty, sy, fy) ->
                    if tx = ty && sx = sy
	  	    then
		      let rec inner i (delta, subst) =
			match subst with
                        | None -> delta, None
                        | Some _ ->
  	                   if i < sx
		           then inner (i+1) (unify (!!(fx i)) (!!(fy i)) (delta, subst))
		           else delta, subst
                      in
		      inner 0 (delta, s)
                    else delta, None
	         | Invalid n, _
                 | _, Invalid n -> invalid_arg (Printf.sprintf "Invalid values for unification (%d)" n)
	         | _ -> delta, None
	        )
      in
      unify x y ([], subst)

  end

module State =
  struct
    type t = Env.t * Subst.t * Subst.t list
    let empty () = (Env.empty (), Subst.empty, [])
    let env   (env, _, _) = env
    let show  (env, subst, constr) =
      sprintf "st {%s, %s, %s}"
              (Env.show env) (Subst.show subst)
              (* (GT.show(GT.list) Subst.show constr) *)
              "<showing list of constraints should be implemented with implicits>"
  end



module Make (Logger: LOGGER) = struct
module Logger = Logger
type state = State.t * Logger.t * Logger.node

type goal = state -> state Stream.t

let make_leaf msg (_,root,from) =
  let dest = Logger.make_node root in
  Logger.connect root from dest msg

  let adjust_state msg (st,root,l) =
    let dest = Logger.make_node root in
    Logger.connect root l dest msg;
    (st,root,dest)

  let (<=>)  : string -> (state -> 'b) -> (state -> 'b)
    = fun msg f st -> f (adjust_state msg st)

  let call_fresh = fun f (((env,s,ss), _, _) as state) ->
    let x, env' = Env.fresh env in
    let new_st = (env',s,ss) in
    ((sprintf "fresh variable '%s'" (generic_show !!x)) <=>
      (fun (_,l,dest) -> f x (new_st, l, dest) ))
      state

  let call_fresh_named name f (( (env,s,ss), _, _) as state) =
    let x, env' = Env.fresh env in
    let new_st = (env',s,ss) in
    ((sprintf "fresh variable '%s' as '%s'" (generic_show !!x) name) <=>
      (fun (_,l,dest) -> f x (new_st, l, dest) ))
      state


let succ prev f = call_fresh (fun x -> prev (f x))

let zero  f = f
let one   f = succ zero f
let two   f = succ one f
let three f = succ two f
let four  f = succ three f
let five  f = succ four f

let q     = one
let qr    = two
let qrs   = three
let qrst  = four
let pqrst = five

exception Disequality_violated

let snd3 (_,x,_) = x

let (===) x y st =
  let (((env, subst, constr), root, l) as state1) =
    st |> adjust_state @@ sprintf "unify '%s' and '%s'"
                                  (show_logic_naive !!x) (show_logic_naive !!y)
  in
  try
    let prefix, subst' = Subst.unify env x y (Some subst) in
    begin match subst' with
    | None ->
       make_leaf "Failed" state1;
       Stream.nil
    | Some s ->
        try
          (* TODO: only apply constraints with the relevant vars *)
          let constr' =
            List.fold_left (fun css' cs ->
              let x, t  = Subst.split cs in
	      try
                let p, s' = Subst.unify env (!!x) (!!t) subst' in
                match s' with
	        | None -> css'
	        | Some _ ->
                    match p with
	            | [] -> raise Disequality_violated
	            | _  -> (Subst.of_list p)::css'
	      with Occurs_check -> css'
            )
            []
            constr
	  in
          let (_,root,_) =
            state1 |> adjust_state @@ sprintf "Success: subs=%s" (Subst.show s) in
          Stream.cons ((env, s, constr'),root,l) Stream.nil
        with Disequality_violated ->
          let () = make_leaf "Disequality violated" state1 in
          Stream.nil
    end
  with Occurs_check ->
    let () = make_leaf "Occurs check failed" state1 in
    Stream.nil

let (=/=) x y (((env, subst, constr) as st),root,l) =
  let normalize_store prefix constr =
    let subst  = Subst.of_list prefix in
    let prefix = List.split (List.map (fun (_, x, t) -> (x, t)) prefix) in
    let subsumes subst (vs, ts) =
      try
        match Subst.unify env !!vs !!ts (Some subst) with
	| [], Some _ -> true
        | _ -> false
      with Occurs_check -> false
    in
    let rec traverse = function
    | [] -> [subst]
    | (c::cs) as ccs ->
	if subsumes subst (Subst.split c)
	then ccs
        else if subsumes c prefix
             then traverse cs
             else c :: traverse cs
    in
    traverse constr
  in
  try
    let prefix, subst' = Subst.unify env x y (Some subst) in
    match subst' with
    | None -> Stream.cons (st,root,l) Stream.nil
    | Some s ->
        (match prefix with
        | [] -> Stream.nil
        | _  -> Stream.cons ((env, subst, normalize_store prefix constr),root,l) Stream.nil
        )
  with Occurs_check -> Stream.cons (st,root,l) Stream.nil

  let conj f g = "conj" <=> (fun st -> Stream.bind (f st) g)

  let (&&&) = conj

  let disj f g =
    (* When call mplus the 1st argument is evaluated earlier, so it will gives answers
       easrlier too *)
    "disj" <=> (fun st -> Stream.mplus (f st) (g st) )


  let (|||) = disj

  let rec (?|) = function
    | [h]  -> h
    | h::t -> h ||| ?| t

  let rec (?&) = function
    | [h]  -> h
    | h::t -> h &&& ?& t

  let conde = (?|)

  let run l f =
    let root_node = Logger.make_node l in
    f (State.empty (), l, root_node)

type diseq = Env.t * Subst.t list

let refine (e, s, c) x = (Subst.walk' e (!!x) s, (e, c))

let reify (env, dcs) = function
| (Var xi) as v ->
    List.fold_left (fun acc s ->
      match Subst.walk' env (!!v) s with
      | Var yi when yi = xi -> acc
      | t -> t :: acc
    )
    []
    dcs
| _ -> []

let take = Stream.take
let take' ?(n=(-1)) stream = List.map (fun (x,_,_) -> x) (Stream.take ~n stream)

end


(* Maybe need copy paste logging *)
          (*
  let (===) x y ((env, subst), l, root) =
    (* LOG[trace1] (logf "unify '%s' and '%s' in '%s' = "
                       (generic_show !!x)
                       (generic_show !!y) (State.show (env, subst))); *)
    match Subst.unify env x y (Some subst) with
    | None   ->
        let dest = Logger.make_node l in
        Logger.connect l root dest (sprintf "unify '%s' and '%s' in '%s' failed" (generic_show !!x) (generic_show !!y) (State.show (env, subst)) );
        Stream.nil
    | Some new_s ->
        let dest = Logger.make_node l in
        Logger.connect l root dest (sprintf "unify '%s' and '%s' in '%s' = '%s'" (generic_show !!x) (generic_show !!y) (State.show (env, subst)) (State.show (env, new_s)) );
        Stream.cons ((env, new_s), l, root) Stream.nil
        *)
