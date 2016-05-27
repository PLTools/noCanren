open Printf

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
        let f = if t = double_array_tag then Obj.magic double_field else field in
        Boxed (t, size x, f x)
      else Invalid t
  )

let generic_show x =
  let x = Obj.repr x in
  let b = Buffer.create 1024 in
  let rec inner o =
    match wrap o with
    | Invalid n                     -> bprintf b "<invalid %d>" n
    | Unboxed n when Obj.magic n=0  -> Buffer.add_string b "[]"
    | Unboxed n                     -> bprintf b "int<%d>" (Obj.magic n)
    | Boxed (t,l,f) when t=0 && l=1 &&
        (match wrap (f 0) with Unboxed i when Obj.magic i >=10 -> true | _ -> false) ->
       bprintf b "var%d"
               (match wrap (f 0) with Unboxed i -> Obj.magic i | _ -> failwith "shit")

    | Boxed   (t, l, f) ->
      bprintf b "boxed %d <" t;
      for i = 0 to l - 1 do (inner (f i); if i<l-1 then Buffer.add_string b " ") done;
      Buffer.add_string b ">"
  in
  inner x;
  Buffer.contents b

module Stream =
struct

  type 'a t = Nil | Cons of 'a * 'a t | Lazy of 'a t Lazy.t

  let from_fun (f: unit -> 'a t) : 'a t =
    let l = Lazy.from_fun f in
    (* print_endline "forcing"; *)
    (* let _ = Lazy.force l in *)
    Lazy l
    (* Lazy (Lazy.from_fun f) *)

  let nil = Nil

  let cons h t =
    (* printf "calling cons of '%s' '%s'\n%!" (generic_show h) (generic_show t); *)
    Cons (h, t)

  let rec take ?(n=(-1)) s =
    (* printf "Stream.take: %s +%d\n%!" __FILE__ __LINE__; *)
    if n = 0
    then []
    else
      (* let () = printf "s = '%s'\n%!" (generic_show s) in *)
      match s with
      | Nil          -> []
      | Cons (x, xs) -> x :: take ~n:(n-1) xs
      | Lazy  z      ->
         (* printf "%s +%d\n%!" __FILE__ __LINE__; *)
         (* printf "%s\n%!" (generic_show @@ Lazy.force z); *)
        take ~n:n (Lazy.force z)

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
    (* printf "calling map recursively\n%!"; *)
    from_fun (fun () ->
      match xs with
      | Nil -> (* print_endline "got Nil";  *)Nil
      | Lazy z -> map f (Lazy.force z)
      | Cons (x,xs) ->
         (* printf "going to call map on head\n%!"; *)
         (* printf "calling f x\n%!"; *)
         let h = f x in
         let tl = map f xs in
         cons h tl
         (* cons (f x) (map f xs) *)
    )

  let rec concat xs ys =
    from_fun (fun () ->
      match xs with
      | Nil -> ys
      | Lazy z -> concat (Lazy.force z) ys
      | Cons (x,xs) -> cons x (concat xs ys)
    )

  let force_once = function
    | Nil -> Nil
    | (Cons _) as y -> y
    | Lazy z -> Lazy.force z

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

type 'a logic = Var of int * 'a logic list | Value of 'a * ('a -> string)

let var_of_int index = Var (index, [])

let const_not_implemented _ =   "<printer not implemented>"
let (!) x = Value (x, const_not_implemented)
let embed {S : ImplicitPrinters.SHOW} x = Value (x, S.show)
(* let embed {S : ImplicitPrinters.SHOW} x = Value (x, generic_show) *)
let inj = embed

let embed_explicit printer x = Value (x, printer)

module Show_logic_explicit (X : ImplicitPrinters.SHOW) = struct
  type t = X.t logic

   let show l =
    match l with
    | Var (index,_) -> sprintf "_.%d" index
    | Value (x,_) -> X.show x

end

implicit module Show_logic {X : ImplicitPrinters.SHOW} =
struct
  type t = X.t logic
  let show l =
    let module P = Show_logic_explicit(X) in
    P.show l
end

let fprintf_logic_with_cs ppf l =
  let open Format in
  let rec print ppf l =
    match l with
    | Var (n,[]) -> fprintf ppf "_.%d" n
    | Var (n, cs) ->
       fprintf ppf "_.%d [=/=" n;
       List.iter (fun l -> fprintf ppf " "; print ppf l) cs;
       fprintf ppf "]"
    | Value (s, f) -> fprintf ppf "%s" (f s)
  in
  print ppf l

let show_logic_with_cs what =
  let b = Buffer.create 10 in
  let fmt = Format.formatter_of_buffer b in
  fprintf_logic_with_cs fmt what;
  Format.pp_print_flush fmt ();
  Buffer.contents b

let show_logic_naive =
  function
  | Var (index,_) -> sprintf "_.%d" index
  | Value (x,printer) ->
    (* TODO: add assert that printer is a function *)
    (* but fix js_of_ocaml before doing that *)
    printer x

let sprintf_logic () x = show_logic_naive x
let fprintf_logic fmt l = Format.fprintf fmt "%s" (sprintf_logic () l)

type 'a llist = Nil | Cons of 'a logic * 'a llist logic

let const_empty_list_str _ = "[]"
let llist_nil = Value(Nil, const_empty_list_str)

let llist_is_empty x = (x=Nil)

let llist_is_empty_logic = function
  | Value (Nil,_) -> true
  | _ -> false

let llist_printer v =
  let b = Buffer.create 49 in
  let rec helper = function
    | Cons (h, tl) when llist_is_empty_logic tl ->
       Buffer.add_string b "[";
       Buffer.add_string b (show_logic_naive h);
       Buffer.add_string b "]"
    | Cons (h,tl) -> begin
        Buffer.add_string b (show_logic_naive h);
        Buffer.add_string b " :: ";
        match tl with
        | Var _ -> Buffer.add_string b (show_logic_naive tl)
        | Value (v,pr) -> Buffer.add_string b (pr v)
      end
    | Nil -> Buffer.add_string b "[]"
  in
  helper v;
  Buffer.contents b

let fprintf_llist fmt ll = Format.fprintf fmt "%s" (llist_printer ll)

module Show_llist_explicit (X: ImplicitPrinters.SHOW) = struct
  type t = X.t llist
  let show = llist_printer
end

implicit module Show_llist {X: ImplicitPrinters.SHOW} = struct
  type t = X.t llist
  let show = llist_printer
end

(* let () = *)
(*   let open ImplicitPrinters in *)
(*   let (_) = embed (Nil: int llist) in *)
(*   () *)

let (%)  x y =
  Value (Cons (x, y), llist_printer)

let (%<) x y =
  let ans = Value( Cons(y,llist_nil), llist_printer) in
  let ans = Value( Cons(x,ans), llist_printer) in
  ans

let (!<) x   =
  (* let printer = function *)
  (*   | Cons (v, Value (Nil,_)) -> sprintf "[%s]" (show_logic_naive v) *)
  (*   | _ -> assert false *)
  (* in *)

  (* WTF!
   * we can't use custom print function there, because it will not pass
   * unification (two values with tag 247 but not physically equal -> no comparable
   * I'm not really sure which workaround exists except using the same function everywhere
   *)
  Value (Cons (x, llist_nil), llist_printer)

let of_list {S : ImplicitPrinters.SHOW} xs =
  let rec helper = function
    | [] -> llist_nil
    | x::xs -> (Value (x, S.show)) % (helper xs)
  in  helper xs

let of_list_hack {S : ImplicitPrinters.SHOW} xs =
  let rec helper = function
    | [] -> Nil
    | x::xs -> Cons (Value(x, S.show), embed (helper xs) )
  in  helper xs

exception Not_a_value
exception Occurs_check

let is_value = function Value _ -> true | Var _ -> false
let to_value_exn = function Var _ -> raise Not_a_value | Value (x,_) -> x

let rec to_listk k = function
| Value (Nil,_) -> []
| Value (Cons (Value (x,_), xs),_) -> x :: to_listk k xs
| z -> k z

let to_list l = to_listk (fun _ -> raise Not_a_value) l


module Env :
  sig
    type t

    val empty  : unit -> t
    val fresh  : t -> 'a logic * t
    val var    : t -> 'a logic -> int option
    val vars   : t -> unit logic list
    val show   : t -> string

    val iter   : t -> ('a logic -> unit) -> unit
  end =
  struct
    module H = Hashtbl.Make (
      struct
        type t = unit logic
        let hash = Hashtbl.hash
        let equal a b = a == b
          (* match a,b with *)
          (* | Var n, Var m when m=n -> true *)
          (* | Var _, Var _ -> false *)
          (* | _ -> a == b *)
      end)

    type latest_counter = int
    type t = unit H.t * latest_counter

    let iter : t -> ('a logic -> unit) -> unit = fun (h,_) f -> H.iter (fun key _v -> f (!!key) ) h

    let vars (h, _) = H.fold (fun v _ acc -> v :: acc) h []

    let show env =
      let f = function
        | (Var (i,_)) as v -> sprintf "_.%d (%d); " i (Hashtbl.hash v)
        | Value _ -> assert false
      in
      sprintf "{ %s }" (String.concat " " @@ List.map f (vars env))

    let counter_start = 10 (* 1 to be able to detect empty list *)
    let empty () = (H.create 1024, counter_start)

    let fresh (h, current) =
      (* let rec v = Var {index=current; reifier=fun () -> (v,[]) } in *)
      let v = Var (current,[]) in
      H.add h v ();
      assert (H.mem h v);
      (* assert (H.mem h !!(Var current)); *)
      (!!v, (h, current+1))

    let var (h, _) x =
      (* printf "calling Env.var when x='%s'\n%!" (generic_show x); *)
      if H.mem h (!! x)
      then
        match !!x with
        | Var (index,_) -> Some index
        | Value _ -> failwith "Value _ should not get to the environment"
      else
        None

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

    (* substitutions are stored as map of pairs key -> (_, to) *)
    type t = (Obj.t * Obj.t) M.t


    let show m =
      let b = Buffer.create 40 in
      (* let open ImplicitPrinters in *)
      bprintf b "subst {";
      M.iter (fun ikey (_, x) ->
          (* printf "inside M.iter x ~= %s\n%!" (generic_show !!x); *)
          bprintf b "%s -> " (show_logic_naive (var_of_int ikey));
          bprintf b "%s; "   (show_logic_naive !!x (* (Value (fst !!x, snd !!x)) *) )
        ) m;
      bprintf b "}";
      Buffer.contents b


    let empty = M.empty

    let of_list l = List.fold_left (fun s (i, v, t) -> M.add i (v, t) s) empty l

    let split s = M.fold (fun _ (x, t) (xs, ts) -> x::xs, t::ts) s ([], [])

    (* [walk e x subs] returns variable which should be substituted according to [x] in substitution [subs] *)
    let rec walk env var subst =
      (* printf "walk _env ~var:'%s' ~subst:'%s'\n%!" (generic_show !!var) (show subst); *)
      match Env.var env var with
      | None   -> var
      | Some i ->
          try walk env (snd (M.find i (!! subst))) subst
          with Not_found -> var

    let rec occurs env xi term subst =
      (* printf "occurs?%!"; *)
      (* printf " _ ~xi:'%s' ~term:'%s' ~subst:'%s'\n%!" (generic_show !!xi) (generic_show !!term) (show subst); *)
      let y = walk env term subst in
      match Env.var env y with
      | Some yi -> xi = yi
      | None ->
         let wy = wrap (Obj.repr y) in
         match wy with
         | Unboxed _ -> false
         | Invalid 247 -> false
         | Invalid n -> invalid_arg (sprintf "Invalid value in occurs check (%d)" n)
         | Boxed (_, s, f) ->
            let rec inner i =
              if i >= s then false
              else occurs env xi (!!(f i)) subst || inner (i+1)
            in
            inner 0

    let rec walk' env var subst =
      (* printf "\nwalk' env:'%s' var:'%s' subst:'%s'\n%!" (Env.show env) (generic_show var) (show subst); *)
      match Env.var env var with
      | None ->
           let v = var in
           (match wrap (Obj.repr v) with
           | Invalid n when n = Obj.closure_tag -> !!var
           | Unboxed _                          -> !!var
           | Invalid n    -> invalid_arg (sprintf "Invalid value for reconstruction (%d)" n)
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
               !!var
          )

      | Some i ->
         (* let () = printf "index = %d\n%!" i in *)
         (* let () = printf "%s +%d: subst = '%s'\n%!" __FILE__ __LINE__ (show subst) in *)
         (* let () = print_endline @@ generic_show @@ (M.find i (!! subst)) in *)
         (* let () = print_endline @@ generic_show (snd (M.find i (!! subst)) ) in *)
         (* let () = flush stdout in *)
         (try walk' env (snd (M.find i (!! subst))) subst
           with Not_found -> !!var
          )

    let show_option f = function Some x -> "Some "^(f x) | None -> "None"
    let unify env x y subst =
      (* printf "_.11 = '%s'\n%!" (generic_show !!(var_of_int 11)); *)
      (* printf "unify: env ~x:'%s' ~subst:'%s'\n%!" (generic_show !!x) *)
      (*        (show_option show subst); *)
      (* printf "_____:     ~y:'%s'\n%!" (generic_show !!y) ; *)
      (* printf ".....      ~x:'%s' ~y:'%s'\n%!" (show_logic_naive x) (show_logic_naive y); *)

      let rec unify x y (delta, subst) =
        (* printf "UNIFY x = '%s'\n%!" (generic_show !!x); *)
        (* printf "      y = '%s'\n%!" (generic_show !!y); *)
        let extend xi x term delta subst =
          if occurs env xi term subst then raise Occurs_check
          else
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
               if x==y
               then (* let () = printf "ptrs are equal\n%!" in *) (delta,s)
               else
                let wx, wy = wrap (Obj.repr x), wrap (Obj.repr y) in
                (match wx, wy with
                 | Unboxed vx, Unboxed vy -> if vx = vy then delta, s else delta, None
                 | Invalid 247, Invalid 247 (* when x==y *) -> delta, s

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
      sprintf "st {%s, %s, [%s]}"
              (Env.show env) (Subst.show subst)
              (* (GT.show(GT.list) Subst.show constr) *)
              (String.concat "; " @@ List.map Subst.show constr)
              (* "<showing list of constraints should be implemented with implicits>" *)
  end

let fst3 (x,_,_) = x

module Make (Logger: LOGGER) = struct
  module Logger = Logger
  type state = State.t * Logger.t * Logger.node

  let describe_log (_,log,node) = (log,node)

  let concrete : state -> State.t = fst3

  type goal = state -> state Stream.t

  let delay_goal: (unit -> goal) -> goal =
    fun f -> fun st -> Stream.from_fun @@ (fun () -> f () st)

  let make_leaf msg (_,root,from) =
    let dest = Logger.make_node root in
    Logger.connect root from dest msg

  let adjust_state msg (st,root,l) =
    let dest = Logger.make_node root in
    Logger.connect root l dest msg;
    (st,root,dest)

  let (<=>)  : string -> (state -> 'b) -> (state -> 'b)
    = fun msg f st -> f (adjust_state msg st)

  let call_fresh: ('a logic -> state -> 'b) -> state -> 'b = fun f (((env,s,ss), _, _) as state) ->
    (* print_endline "call_fresh"; *)
    let x, env' = Env.fresh env in
    let new_st = (env',s,ss) in
    ((sprintf "fresh variable '%s'" (show_logic_naive x)) <=>
      (fun (_,l,dest) -> f x (new_st, l, dest) ))
      state

  let call_fresh_named name f (( (env,s,ss), _, _) as state) =
    let x, env' = Env.fresh env in
    let new_st = (env',s,ss) in
    ((sprintf "fresh variable '%s' as '%s'" (show_logic_naive x) name) <=>
     (fun (_,l,dest) -> f x (new_st, l, dest) ))
      state

exception Disequality_violated

let snd3 (_,x,_) = x

let (===) x y st =
  (* printf "call (%s) === (%s)\n%!" (show_logic_naive x) (show_logic_naive y); *)
  let (((env, subst, constr), root, l) as state1) =
    st |> adjust_state @@ sprintf "unify '%s' and '%s'"
                                  (show_logic_naive x) (show_logic_naive y)
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
          (* print_endline "folding constraints"; *)
          (* printf "constr = '%s'\n%!" (generic_show constr); *)
          let constr' =
            List.fold_left (fun css' cs ->
              let (x, t) : Obj.t list * Obj.t list  = Subst.split cs in
              (* printf "css' = '%s', cs='%s'\n%!" (generic_show !!css') (Subst.show cs); *)
              (* printf "x = [%s], t=[%s]\n%!" *)
              (*   (String.concat "; " @@ List.map (fun x -> generic_show !!x) x) *)
              (*   (String.concat "; " @@ List.map (fun x -> generic_show !!x) t); *)

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

          let ans = Stream.cons ((env, s, constr'),root,l) Stream.nil in
          (* printf "%s +%d\n%!" __FILE__ __LINE__; *)
          (* printf "ans = '%s'\n%!" (generic_show ans); *)
          ans
        with Disequality_violated ->
          let () = make_leaf "Disequality violated" state1 in
          Stream.nil
    end
  with Occurs_check ->
    let () = make_leaf "Occurs check failed" state1 in
    Stream.nil

let (=/=) x y state0 =
  let ( ((env,subst,constr),root,l) as state1) =
    adjust_state (sprintf "checking '%s' =/= '%s'"
                    (show_logic_naive x) (show_logic_naive y)
                 ) state0
  in
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
  let string_of_prefixes xs =
    String.concat "; " @@ List.map (fun (x,y,z) -> sprintf "(%d,'%s','%s')" x (generic_show !!y) (generic_show !!z) ) xs
  in
  try
    match Subst.unify env x y (Some subst) with
    | (_,None) ->
        let () = make_leaf "Nothing added: can't unify" state1 in
        Stream.cons state1 Stream.nil
    | (prefix,Some s) ->
        (* let (_:int) = prefix in *)
        (match prefix with
         | [] ->
             let () = make_leaf (sprintf "Unified but prefix is empty (subst: %s)" (Subst.show s) ) state1 in
             Stream.nil
         | _  ->
             let () = make_leaf (sprintf "Adding new answer (%s) with constraints: [%s]" (Subst.show subst) (string_of_prefixes prefix)) state1 in
             Stream.cons ((env, subst, normalize_store prefix constr),root,l) Stream.nil
        )
  with Occurs_check ->
    let () = make_leaf (sprintf "Occurs_check failed") state1 in
    Stream.cons state1 Stream.nil

  let conj f g = "conj" <=>
    (fun ((_,_,l0) as st) -> Stream.bind (f st) (fun (s,l,_) -> g (s,l,l0)) )

  let (&&&) = conj

  let disj f g st0 =
    let st = adjust_state "|||" st0 in
    (* When call mplus the 1st argument is evaluated earlier, so it will gives answers
       easrlier too *)
    Stream.mplus (f st) Stream.(from_fun (fun () -> g st) )

  let (|||) = disj

  let rec (?|) = function
    | [] -> failwith "not implemented. should not happen"
    | [h]  -> h
    | h::t -> h ||| ?| t

  let rec (?&) = function
    | [] -> failwith "not implemented. should not happen"
    | [h]  -> h
    | h::t -> h &&& ?& t

  let conde = (?|)

  let first_of : goal list -> goal = fun xs st0 ->
    let open Stream in
    let rec helper xs = function
      | Nil        -> helper2 xs
      | Lazy l     -> helper xs (Lazy.force l)
      | Cons (x,y) -> Cons (x,y)
    and helper2 = function
      | [] -> assert false
      | [f] -> Stream.from_fun (fun () -> f st0)
      | f::fs -> helper fs (f st0)
    in
    match xs with
    | [] -> failwith "first_of can't take empty list"
    | [f] -> helper2 [f]
    | x::xs -> helper xs (x st0)

  let run ?(logger=Logger.create()) (g: state -> _) =
    let root_node = Logger.make_node logger in
    g (State.empty (), logger, root_node)

  type diseq = Env.t * Subst.t list

  (* let refine (e, s, c) x = (Subst.walk' e (!!x) s, (e, c)) *)
let rec refine : 'a . State.t -> 'a logic -> 'a logic = fun ((e, s, c) as st) x ->
  let (
  let rec walk' env var subst =
    let var = Subst.walk env var subst in
    match Env.var env var with
    | None ->
        (match wrap (Obj.repr var) with
         | Unboxed _ -> !!var
         | Boxed (t, s, f) ->
            let var = Obj.dup (Obj.repr var) in
            let sf =
              if t = Obj.double_array_tag
              then !! Obj.set_double_field
              else Obj.set_field
            in
            for i = 0 to s - 1 do
              sf var i (!!(walk' env (!!(f i)) subst))
           done;
           !!var
         | Invalid n -> invalid_arg (Printf.sprintf "Invalid value for reconstruction (%d)" n)
        )
    | Some i ->
        (match var with
         | Var (i, _) ->
            let cs =
	      List.fold_left
		(fun acc s ->
		   match Subst.walk' env (!!var) s with
		   | Var (j, _) when i = j -> acc
		   | t -> (refine st t) :: acc
		)
		[]
		c
	    in
	    Var (i, cs)
        )
  in
  walk' e (!!x) s

  let reify (env, dcs) = function
    | (Var (xi,_)) as v ->
       List.fold_left
         (fun acc s ->
          match Subst.walk' env (!!v) s with
          | Var (yi,_) when yi == xi -> acc
          | t -> t :: acc
         )
         []
         dcs
    | _ -> []

  let take = Stream.take
  let take' ?(n=(-1)) stream = List.map (fun (x,_,_) -> x) (Stream.take ~n stream)

  let run_ ?(logger=Logger.create()) = run ~logger


  module PolyPairs = struct
    let id x = x

    let find_value var st = refine st var
    let mapper var = fun stream -> Stream.map (find_value var) stream

    type 'a reifier = 'a logic Stream.t

    let one: ('a reifier -> 'b) -> state Stream.t -> 'a logic -> 'b =
      fun k stream var -> k (mapper var stream)
    let succ prev k = fun stream var -> prev  (fun v -> k (mapper var stream, v)) stream

    (* let (_: state Stream.t -> *)
    (*      'a logic -> *)
    (*      'b logic -> *)
    (*      ('a reifier) * ('b reifier) ) = (succ one) id *)

    let p sel = sel id
  end

  type 'a result = 'a logic
(*
  let refine' : 'a . State.t -> 'a logic -> 'a result =
    fun ((e, s, c)as st) x ->
      (* printf "calling refine' for state %s\n%!" (State.show st); *)
      (Subst.walk' e (!!x) s, reify (e, c) x)
      *)
  let (dummy_goal2: int logic -> string logic -> goal) = fun _ _ _   -> Obj.magic ()

  module ExtractDeepest = struct
    let ext2 (a, base) = (a,base)
    let ext3 (a,(b,base)) = ((a,b),base)

    let succ prev (a,z) =
      let (foo,base) = prev z in
      ((a,foo), base)

    let ext4 x = succ ext3 x
  end

  module ApplyTuple = struct
    let one arg x = x arg
    let two arg (x,y) = (x arg,y arg)

    let succ prev = fun arg (x,y) -> (x arg, prev arg y)
  end

  module ApplyLatest = struct
    let two = (ApplyTuple.one, ExtractDeepest.ext2)
    let three = (ApplyTuple.(fun x -> succ one x), ExtractDeepest.(fun x -> succ ext2 x))

    let apply (appf, extf) tup =
      let (x,base) = extf tup in
      appf base x

    let succ (appf, extf) = (ApplyTuple.(succ appf), ExtractDeepest.(succ extf) )
  end

  module MyCurry = struct
    let succ k f x = k (fun y -> f (x,y))
    let one       = (@@)
    let two   x   = succ one x
    let three x y = succ two x y
  end

  module MyUncurry = struct
    let succ k f (x,y) = k (f x) y
    let uncurry1 = (@@)
  end

  module ConvenienceCurried = struct
    type 'a reifier = (Logger.t * 'a logic) Stream.t
    type 'a almost_reifier = state Stream.t -> 'a reifier

    let reifier : 'a logic -> 'a almost_reifier = fun x ans ->
      Stream.map (fun (st,root,_) -> (root, refine st x) ) ans

    module LogicAdder = struct
      (* allows to add new logic variables to function *)
      let zero  f = f

      let succ (prev: 'a -> state -> 'b) (f: 'c logic -> 'a)
        : state -> 'c almost_reifier * 'b =
        call_fresh (fun logic st ->
            (reifier logic, prev (f logic) st)
          )
    end

    let one = (fun x -> LogicAdder.(succ zero) x), MyCurry.one, ApplyLatest.two

    let succ (adder,currier, app) = (LogicAdder.succ adder,
                                     MyUncurry.succ currier,
                                     ApplyLatest.succ app)

    let run (adder,currier,app_num) goalish f =
      run_ (adder goalish) |> ApplyLatest.(apply app_num) |> (currier f)

    let (_: ('a reifier -> 'b reifier -> 'c) -> 'c ) =
      run (succ one) dummy_goal2
  end

  module ConvenienceStream = struct

    module LogicAdder = struct
      (* allows to add new logic variables to function *)
      let zero  f = f

      let succ (prev: 'a -> state -> 'b) (f: 'c logic -> 'a) : state -> 'c logic * 'b =
        call_fresh (fun logic st -> (logic, prev (f logic) st) )
    end
    module Refine = struct
      let one state x = refine' state x
      let succ prev = fun state (x,y) -> (refine' state x, prev state y)
    end

    let one
       = (fun x -> LogicAdder.(succ zero) x), ExtractDeepest.ext2, Refine.one

    let succ (prev,extD,af) =
      (LogicAdder.succ prev, ExtractDeepest.succ extD, Refine.succ af)

    let run (adder,extD,appF) goalish =
      let tuple,stream = run_ (adder goalish) |> extD in
      (* printf "run %s +%d\n%!" __FILE__ __LINE__; *)
      (* printf "stream = '%s'\n%!" (generic_show stream); *)
      let ans =
        Stream.map (fun (st: state) ->
          (* print_endline "inside mapper function"; *)
          let a1 = snd3 st in
          (* printf "a1 ready %s +%d\n%!" __FILE__ __LINE__; *)
          (* print_endline @@ generic_show @@ fst3 st; *)
          let b1 = appF (fst3 st) tuple in
          let rez = a1,b1 in
          (* printf "rez ready\n%!"; *)
          rez
        ) stream
      in
      let _ = Stream.take ~n:1 ans in
      (* printf "run finishes %s +%d\n%!" __FILE__ __LINE__; *)
      ans

    let (_: (Logger.t * ((int logic * int logic_diseq) * (string logic * string logic_diseq))) Stream.t)
       = run (succ one) dummy_goal2

  end
end



(*
module Test1 = struct
  open Printf
  open ImplicitPrinters

  module M = Make(UnitLogger)
  open M

  let (!) = embed

  module Nat = struct
    type t = O | S of t logic
    let show = function
      | O -> "O"
      | S n -> sprintf "S (%s)" (show_logic_naive n)
  end
  implicit module Show_nat : (SHOW with type t = Nat.t) = Nat

  let nat_of_int n : Nat.t =
    if n<0 then failwith "bad argument"
    else
    let rec helper acc n =
      if n=0 then acc
      else helper (Nat.S !acc) (n-1)
    in
    helper Nat.O n

  let is_positive_nat n = call_fresh (fun _zero -> n === !(Nat.S _zero))
  let is_nonnegative_nat n =
    call_fresh (fun _zero -> (n === !(Nat.S _zero)) ||| (n === !Nat.O) )

  module Peano_int = struct
    type t = bool * Nat.t logic
    let show : t -> string = fun (p,n) ->
      if p then show n
      else "-" ^ (show n)
    let of_int n =
      if n>=0 then (true, !(nat_of_int n) )
      else (false, !(nat_of_int (-n)) )
  end
(*
let is_positive_peano p =
  let open Nat in
  call_fresh (fun _zero -> p === !(true, !(S _zero)) )

let is_negative_peano p =
  let open Nat in
  fresh (_zero) (p === !(false, !(S _zero)) )

let is_nonnegative_peano p =
  let open Nat in
  conde [ p === !(true, !O)
        ; p === !(false, !O)
        ; is_positive_peano p
        ]
 *)
module MiniLambda = struct
  type structured_constant = Peano_int.t
  and lambda =
    | Lconst of structured_constant logic
end

implicit module Show_MiniLambda : (SHOW with type t = MiniLambda.lambda) =
struct
  type t = MiniLambda.lambda
  let show =
    let open MiniLambda in
    let rec helper = function
      | Lconst l -> sprintf "Lconst %s" (show_logic_naive l)
    in
    helper
end

let eval_lambda (lam_ast: MiniLambda.lambda) =
  let open MiniLambda in
  let rec evalo l (ans: MiniLambda.lambda logic) st =
    printf "evalo '%s' '%s'\n%!" (show l) (show ans);
    (ans === l) st
  in
  (* let open M.ConvenienceCurried in *)
  (* let open ImplicitPrinters in *)

  let naive () =

    let ans =
      run_ (call_fresh (fun q st -> (evalo !(Lconst !(Peano_int.of_int 1))) q st,q) )
    in
    (* let (stream, q) = ans in *)
    (* let _ = Stream.take ~n:1 stream in *)
    (* let xs = Stream.take ~n:1 stream *)
    (*          |> List.map (fun ((e,_st,constr),_,_) -> reify (e,constr) q) *)
    (* in *)
    (* printf "answers: %d\n%!" (List.length xs); *)
    (* List.iter (fun x -> print_endline @@ show x) xs *)
    ()
  in
  let streams () =
    let open ConvenienceStream in
    let adder,extD,af = one in

    let ans =
      adder (fun q st -> (evalo !(Lconst !(Peano_int.of_int 1))) q st)
    in
    let (q,stream) = run_ ans in
    let _ = Stream.take ~n:1 stream in
    (* let (_:int) = q in *)
    let xs = Stream.take ~n:1 stream
             (* |> List.map (fun (st,_,_) -> refine st q |> fst) *)
             |> List.map (fun (st,_,_) ->
                 (* printf "=== %s\n%!" (State.show st); *)
                 refine st q
               )
    in
    (* let (__: MiniLambda.lambda logic list) = xs in *)
    (* printf "***************** answers: %d\n%!" (List.length xs); *)
    (* List.iter (fun x -> print_endline @@ show x) xs; *)
    ()
  in
  (* let stream = run one @@ evalo !(Lconst !(Peano_int.of_int 1)) in *)
  (* printf "stream    %s %d\n%!" __FILE__ __LINE__; *)
  (* printf "stream = '%s'\n%!" (MiniKanren.generic_show stream); *)
  (* let xs = stream |> MiniKanren.Stream.take ~n:1 *)
  (*          |> List.map (fun (_logger, (_q,_constraints)) -> _q) *)
  (* in *)
  (* let xs = stream (fun var1 -> var1 1 |> List.map (fun (_logger, (_q,_constraints)) -> _q) ) *)
  (* in *)
  (* let (_:int list) = xs in *)
  (* let (_q,stream) = Tester.M.run (call_fresh (fun q st ->  evalo !(Lconst !(make_const 1)) q st,q) ) in *)
  (* let _ = Stream.take ~n:1 stream in *)
  (* printf "answers: %d\n%!" (List.length xs); *)
  (* List.iter (fun x -> print_endline @@ show x) xs; *)
  let () = streams () in
  ()

let main () =
  let open MiniLambda in
  let lam2 = Lconst !(Peano_int.of_int 1) in
  let () = eval_lambda lam2 in
  ()

  end


let () = Test1.main ()
         *)
