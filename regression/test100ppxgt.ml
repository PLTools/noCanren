(* to get source:
  mkae ppx && ocamlfind ppx_tools/rewriter "`ocamlfind query ppx_deriving`/ppx_deriving -deriving-plugin _build/ppx/ppx_deriving_gt.cma" regression/test100ppxgt.ml

*)
type token_env = int
(* let () = print_endline @@ GT.show token_env 5 *)

type 'a logic =
| Var   of GT.int * 'a logic GT.list * 'a logic
| Value of 'a
[@@deriving gt {show} ] 

(*
class type virtual ['a,'ia,'sa,'inh,'syn] logic_tt =
  object
    method  c_Var :
      'inh ->
        ('inh,'a logic,'syn,< a: 'ia -> 'a -> 'sa   > ) GT.a ->
          GT.int -> 'a logic GT.list -> 'a logic -> 'syn
    method  c_Value :
      'inh ->
        ('inh,'a logic,'syn,< a: 'ia -> 'a -> 'sa   > ) GT.a ->
          ('ia,'a,'sa,< a: 'ia -> 'a -> 'sa   > ) GT.a -> 'syn
    method  t_logic : ('ia -> 'a -> 'sa) -> 'inh -> 'a logic -> 'syn
  end
let (logic :
  (('ia -> 'a -> 'sa) ->
     ('a,'ia,'sa,'inh,'syn)#logic_tt -> 'inh -> 'a logic -> 'syn,unit)
    GT.t)
  =
  let rec logic_gcata fa trans inh subj =
    let rec self = logic_gcata fa trans

    and tpo = object method a = fa end
     in
    match subj with
    | Var (p0,p1,p2) -> trans#c_Var inh (GT.make self subj tpo) p0 p1 (GT.make self p2 tpo)
    | Value p0 ->
        trans#c_Value inh (GT.make self subj tpo) (GT.make fa p0 tpo)
     in
  { GT.gcata = logic_gcata; GT.plugins = () }
class virtual ['a,'ia,'sa,'inh,'syn] logic_t =
  object (this)
    method virtual  c_Var :
      'inh ->
        ('inh,'a logic,'syn,< a: 'ia -> 'a -> 'sa   > ) GT.a ->
          GT.int -> 'a logic GT.list ->
          ('inh, 'a logic, 'syn, < a : 'ia -> 'a -> 'sa >) GT.a -> 'syn
    method virtual  c_Value :
      'inh ->
        ('inh,'a logic,'syn,< a: 'ia -> 'a -> 'sa   > ) GT.a ->
          ('ia,'a,'sa,< a: 'ia -> 'a -> 'sa   > ) GT.a -> 'syn
    method t_logic fa = (GT.transform logic) fa this
  end
class type ['a] show_logic_env_tt = object  end
class ['a] show_proto_logic env =
  object (this)
    inherit  ['a,unit,string,unit,string] logic_t
    method c_Value inh subj p0 = "Value " ^ (p0.GT.fx ())
    method c_Var inh subj p0 p1 p2 =
      "Var (" ^
        ((String.concat ", "
            [(GT.lift (GT.int.GT.plugins)#show ()) p0;
            (GT.lift
               ((GT.list.GT.plugins)#show
                  (GT.transform logic (subj.GT.t)#a this ())) ()) p1;
            p2.GT.fx ()
            ])
           ^ ")")
  end
class ['a] show_logic_t = let self = Obj.magic (ref ())  in
  object (this)
    inherit  ['a,unit,string,unit,string] logic_t
    inherit  ((['a] show_proto_logic) self)
    initializer self := (this :> 'a show_logic_t)
  end
let logic =
  {
    GT.gcata = ();
    GT.plugins =
      (object method show = GT.transform logic (new show_logic_t) () end)
  }

*)

let () =
  let q = Var (1,[]) in
  let r = Value "s" in
  let s = Var (2,[q;r]) in
  List.iter (fun l -> print_endline @@ GT.show logic l) [q;r;s]

(*
type ('a, 'l) llist = Nil | Cons of 'a * 'l

type 'a lnat = O | S of 'a
 (* [@@deriving show] *)
*)
