open Printf
open MiniKanren
open Tester

type 'a f = ('a, 'a) fancy
type ('varname, 'self) glam =
  | V of 'varname f
  | App of 'self * 'self
  | Abs of 'varname * 'self
  | MetaVar

type rlam = (string, rlam) glam
type lam = (string f, (lam,rlam) fancy) glam

module LamHack = FMapALike0(struct type t = lam type r = rlam end)

let v x   : (lam,rlam) fancy = LamHack.wrap @@ inj @@ lift @@ V x
let app x y = LamHack.wrap @@ inj @@ lift @@ App (x,y)
let abs x y = LamHack.wrap @@ inj @@ lift @@ Abs (x,y)

let show_lam lam =
  let b = Buffer.create 10 in
  let rec helper = function
  | V s -> bprintf b "V "; bprintf_fancy b (bprintf b "\"%s\"") s
  | App (f,m) ->
    bprintf b "App (";
    bprintf_fancy b helper f;
    bprintf b ", ";
    bprintf_fancy b helper m;
    bprintf b ")"
  | Abs (s, e) ->
    bprintf b "Abs (";
    bprintf_fancy b (bprintf b "%s") s;
    bprintf b ", ";
    bprintf_fancy b helper e;
    bprintf b ")"
  | MetaVar -> bprintf b "_.%d" 0
  in
  helper lam;
  Buffer.contents b
;;

@type ('a, 'b) gtyp =
  (* primitive *)
  | P of 'a
  | Arr of 'b * 'b with gmap;;

type ftyp = (string f, ftyp f) gtyp
type rtyp = (string, rtyp) gtyp
type ltyp = (string logic, ltyp) gtyp logic

let p s     : (ftyp,rtyp) fancy = Obj.magic @@ inj @@ lift @@ P s
let arr x y : (ftyp,rtyp) fancy = Obj.magic @@ inj @@ lift @@ Arr (x,y)

(* reifier for types *)
let ltyp_of_ftyp isVar f =
  let cond : 'a -> bool = fun x -> isVar !!!x in
  let rec helper (t: ftyp f) : ltyp =
    if cond t then !!!t
    else match coerce_fancy t with
    | P s -> Value (if cond !!!s then P (var_of_fancy s) else P (Value (coerce_fancy s)))
    | Arr (f,g) ->
      Value (Arr ((if cond f then !!!f else helper f),
                  (if cond g then !!!g else helper g)))
  in
  helper (Obj.obj f)

let show_ftyp typ =
  (* printf "show_typ '%s'\n%!" (generic_show typ); *)
  let b = Buffer.create 10 in
  let rec helper = function
  | P s -> bprintf_fancy b (bprintf b "%s") s
  | Arr (f,m) ->
    bprintf_fancy b helper f;
    bprintf b " -> ";
    bprintf_fancy b helper m
  in
  bprintf_fancy b helper typ;
  Buffer.contents b

let show_rtyp typ =
  let b = Buffer.create 10 in
  let rec helper = function
  | P s -> bprintf b "%s" s
  | Arr (f,m) ->
    helper f;
    bprintf b " -> ";
    helper m
  in
  helper typ;
  Buffer.contents b

let show_ltyp typ =
  (* printf "show_lyp '%s'\n%!" (generic_show typ); *)
  let b = Buffer.create 10 in
  let rec helper = function
  | P s -> bprintf_logic b (bprintf b "%s") s
  | Arr (f,m) ->
    bprintf_logic b helper f;
    bprintf b " -> ";
    bprintf_logic b helper m
  in
  bprintf_logic b helper typ;
  Buffer.contents b


(*
let typ_reifier (cond: Obj.t -> bool) (x : typ) : typ =
  let rec helper x =
    if cond @@ Obj.repr x then TypeVar 0
    else match Obj.magic x with
    | TypeVar n -> TypeVar n
    | Arr (f,x) -> Arr(helper f, helper x)
    | P s when cond @@ Obj.repr s -> TypeVar 0
    | P s -> P s
  in
  helper x
*)
let rec lookupo a g t =
  Fresh.three (fun a' t' tl ->
    (g === (inj_pair a' t')%tl) &&&
    (conde [
      (a' === a) &&& (t' === t);
      lookupo a tl t
     ])
  )

let infero expr typ =
  let rec infero gamma expr typ =
    conde [
      Fresh.one (fun x ->
        (expr === v x) &&&
        (lookupo x gamma typ));
      Fresh.three (fun m n t ->
        (expr === app m n) &&&
        (infero gamma m (arr t typ)) &&&
        (infero gamma n t));
      Fresh.four (fun x l t t' ->
        (expr === abs x l) &&&
        (typ  === arr t t') &&&
        (infero ((inj_pair x t)%gamma) l t'))
    ]
  in
  infero (nil()) expr typ

let show_string = fun x -> x
let show_fstring : (string, string) fancy -> string = show_fancy show_string
(* let show_ftyp: ftyp -> string = show_fancy*/ show_ftyp *)
let show_fpair f g = show_fancy (fun (a,b) -> sprintf "(%s,%s)" (f a) (g b))

(* let (_:int) = (show_fpair show_fstring @@ show_ftyp) *)
let show_env : (((string f * ftyp f, int) fancy, 'a) llist as 'a, int) fancy -> string  =
  MiniKanren.List.show (show_fpair show_fstring show_ftyp)

let varX : (string,string) fancy = inj@@lift "x"
let varY = inj@@lift "y"
let varF = inj@@lift "f"

let inj_list_p xs = inj_list @@ List.map (fun (x,y) -> inj_pair x y) xs

let _noFreeVars =
  run_exn show_lam    1 q (REPR (fun q -> lookupo varX (inj_list []) q                                     )) qh;
  run_exn show_lam    1 q (REPR (fun q -> lookupo varX (inj_list_p [(varX, v varX)]) q                     )) qh;
  run_exn show_lam    1 q (REPR (fun q -> lookupo varX (inj_list_p [(varY, v varY); (varX, v varX)]) q     )) qh;

  run_exn show_string 1 q (REPR (fun q -> lookupo q (inj_list_p [(varY, v varY); (varX, v varX)]) (v varX)   )) qh;
  run_exn show_string 1 q (REPR (fun q -> lookupo q (inj_list_p [(varY, v varY); (varX, v varX)]) (v varY)   )) qh;
  run_exn show_rtyp   1 q (REPR (fun q -> infero (abs varX (app (v varX) (v varX)))                q)) qh;
  ()

let (_: (lam,lam) fancy -> (ftyp, rtyp) fancy -> goal) = infero
let runT n  = runR ltyp_of_ftyp show_rtyp show_ltyp n
let (_ : ((Obj.t -> bool) -> Obj.t -> _) -> (rtyp -> string) -> (ltyp -> string) -> unit) =
  fun r g h -> runR r g h 1 q (REPR (fun q -> infero (abs varX (v varX)) q   )) ~h:qh

let runL n  = runR ltyp_of_ftyp show_rtyp show_ltyp n
let _withFreeVars =
  (*run  show_env    1 q (REPR (fun q -> lookupo varX q (v varY)                                            )) qh; *)
  runT 1 q (REPR (fun q -> infero (abs varX (v varX)) q                )) qh;
  runT     1 q (REPR (fun q -> infero (abs varF (abs varX (app (v varF) (v varX)))) q        )) qh;
  runT     1 q (REPR (fun q -> infero (abs varX (abs varF (app (v varF) (v varX)))) q        )) qh;
  (*run  show_lam    1 q (REPR (fun q -> infero q (arr (p varX) (p varX))                              )) qh; *)
  ()
