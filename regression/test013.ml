(* Many tests for Nat-, Bool- and List- primitives *)
open Printf
open MiniKanren
open Tester

let show_nat        = Nat.show_ground
let show_bool       = Bool.show_ground

let show_nat_llist  = GT.show(List.ground) Nat.show_ground
let show_bool_llist = GT.show(List.ground) (Bool.show_ground)
let show_option_nat = GT.(show option  Nat.show_ground)

let (?$) = inj_nat
let nats = inj_nat_list
let bools bs = inj_list @@ List.map Bool.inj bs

let unitf = inj @@ lift ()

let sumo = List.foldro Nat.addo ?$0

let () =
  run_exn show_bool        1    q  qh (REPR (fun q     -> Bool.noto' Bool.true_  q                       ));
  run_exn show_bool        1    q  qh (REPR (fun q     -> Bool.noto' Bool.false_ q                       ));
  run_exn show_bool        1    q  qh (REPR (fun q     -> Bool.noto' q          Bool.true_               ));
  run_exn show_bool        1    q  qh (REPR (fun q     -> Bool.oro  Bool.false_ Bool.false_ q            ));
  run_exn show_bool        1    q  qh (REPR (fun q     -> Bool.oro  Bool.false_ Bool.true_  q            ));
  run_exn show_bool        1    q  qh (REPR (fun q     -> Bool.oro  Bool.true_  Bool.false_ q            ));
  run_exn show_bool        1    q  qh (REPR (fun q     -> Bool.oro  Bool.true_  Bool.true_  q            ));
  run_exn show_bool        1    q  qh (REPR (fun q     -> Bool.ando Bool.false_ Bool.false_ q            ));
  run_exn show_bool        1    q  qh (REPR (fun q     -> Bool.ando Bool.false_ Bool.true_  q            ));
  run_exn show_bool        1    q  qh (REPR (fun q     -> Bool.ando Bool.true_  Bool.false_ q            ));
  run_exn show_bool        1    q  qh (REPR (fun q     -> Bool.ando Bool.true_  Bool.true_  q            ));
  run_exn show_nat         1    q  qh (REPR (fun q     -> Nat.addo ?$0 ?$1 q                             ));
  run_exn show_nat         1    q  qh (REPR (fun q     -> Nat.addo ?$1 q   ?$3                           ));
  run_exn show_nat         3   qr qrh (REPR (fun q r   -> Nat.addo q   r   q                             ));
  run_exn show_nat         1    q  qh (REPR (fun q     -> Nat.mulo ?$1 ?$2 q                             ));
  run_exn show_nat         1    q  qh (REPR (fun q     -> Nat.mulo ?$3 q   ?$6                           ));
  run_exn show_nat         1    q  qh (REPR (fun q     -> Nat.mulo ?$3 q   ?$6                           ));
  run_exn show_nat         1    q  qh (REPR (fun q     -> Nat.mulo ?$3 ?$0 q                             ));
  run_exn show_nat         1    q  qh (REPR (fun q     -> Nat.mulo q   ?$5 ?$0                           ));
  run_exn show_nat         3    q  qh (REPR (fun q     -> Nat.mulo q   ?$0 ?$0                           ));
  ()

let () =
  run_exn show_nat         1    q  qh (REPR (fun q     -> sumo (nats []) q                               ));
  run_exn show_nat         1    q  qh (REPR (fun q     -> sumo (nats [3;1;2]) q                          ));
  run_exn show_nat         1    q  qh (REPR (fun q     -> sumo (?$0 % (?$1 % (q %< ?$3))) ?$6            ));
  ()

let () =
  run_exn show_nat         1    q   qh (REPR (fun q     -> List.lengtho (nats [1;2;3;4]) q                    ));
  run_exn show_nat         1    q   qh (REPR (fun q     -> List.lengtho (inj_list [unitf; unitf; unitf]) q    ));
  run_exn show_nat         1    q   qh (REPR (fun q     -> List.lengtho (bools [false; true]) q               ));
  run_exn show_nat         1    q   qh (REPR (fun q     -> List.lengtho (nats [4;3;2;1;0]) q                  ));
  run_exn show_nat_llist   1    q   qh (REPR (fun q     -> List.lengtho q ?$0                                 ));

  run_exn show_bool        1    q   qh (REPR (fun q     -> List.anyo (bools [false;false;true]) q         ));
  run_exn show_bool        1    q   qh (REPR (fun q     -> List.anyo (bools [false;false]) q              ));

  run_exn show_bool        1    q   qh (REPR (fun q     -> List.allo (bools [true;false;true]) q          ));
  run_exn show_bool        1    q   qh (REPR (fun q     -> List.allo (Bool.true_ % (q %< Bool.true_)) Bool.true_  ));
  run_exn show_bool      (-1) qrs qrsh (REPR (fun q r s -> List.allo (Bool.true_ % (q %< r)) s                    ));
  ()

let _ =
  run_exn show_nat_llist    1    q  qh (REPR (fun q     -> List.mapo (Nat.addo ?$1) (nats [0;1;2]) q              ));
  run_exn show_nat_llist    1    q  qh (REPR (fun q     -> List.mapo (Nat.addo ?$2) q (nats [4;3;2])              ));
  run_exn show_nat          1    q  qh (REPR (fun q     -> List.mapo (Nat.addo q) (nats [1;2;3]) (nats [4;5;6])   ));
  run_exn show_nat          1    q  qh (REPR (fun q     -> List.mapo (Nat.mulo q) (nats [1;2;3]) (nats [2;4;6])   ));
  run_exn show_nat          1   qr qrh (REPR (fun q r   -> List.mapo (Nat.mulo q) (nats [1;2]) (?$2 %< r)         ));
  run_exn show_nat_llist    1    q  qh (REPR (fun q     -> List.mapo (===) (nats [1;2;3]) q                       ));
  run_exn show_nat          1    q  qh (REPR (fun q     -> List.mapo (===) (nats [1;2;3]) (?$1 % (?$2 %< q))    ));
  run_exn show_bool_llist   1    q  qh (REPR (fun q     -> List.mapo Bool.noto' (bools [true;false;true;]) q    ));
  run_exn show_bool_llist   1    q  qh (REPR (fun q     -> List.mapo Bool.noto' (bools []) q                    ));

  run_exn show_nat_llist  (-1)   q  qh (REPR (fun q     -> List.filtero (eqo ?$2) (nats [0;1;2;3]) q          ));
  run_exn show_option_nat   1    q  qh (REPR (fun q     -> List.lookupo (eqo ?$1) (nats [0;2;1;3]) q          ));
  ()

(*
exception GotANumber of int

let refine_nat (c: var_checker) n : [ `Number of int | `Nat of Nat.logic ] =
  let startInt : int -> int = fun x -> x in
  let startNat : Nat.logic -> Nat.logic = fun n -> Value (S n) in

  let rec helper (makeInt: int -> int) makeNat (n: Nat.ground fancy) =
    if c#isVar n then refine_fancy (injlift n) c (refine_nat_tologic c)
    else match coerce_fancy n with
    | S next -> helper (fun old -> Pervasives.(+) (makeInt old) 1) (fun n -> Value (S (makeNat n))) (injlift  next)
    | O -> raise (GotANumber (makeInt 0))
  in
  try `Nat (helper startInt startNat n)
  with GotANumber n -> `Number n

(* This is intentionally tricky refiner. We suppose that 2nd cons cell is not logic variable
 * and hence we are able to generate plane lists and not logical lists
 * Can be useful for queries like          `lengtho q (of_nat 3)`
 *)
let to_list_lnats (c: var_checker) y =
  let rec helper (t: Nat.ground List.flist) : [ `Number of int | `Nat of Nat.logic ] list =
    if c#isVar t then failwith "This refiner is not supposed to do this"
    else match coerce_fancy t with
    | Nil -> []
    | Cons (h, tl) when c#isVar h ->
        `Nat (refine_fancy (injlift h) c (fun x -> refine_nat_tologic c (injlift x))) :: (helper tl)
    | Cons (h, tl) -> (refine_nat c (injlift h)) :: (helper tl)
  in
  helper y

let show_poly_list xs = GT.show(GT.list) (function
  | `Number n -> string_of_int n
  | `Nat n -> (GT.show(Nat.logic)) n
  ) xs
*)

(* TODO: maybe put fmap into generated reifier by default ? *)
(* let pair_reifier : var_checker -> (string*rlam, _) fancy -> _ = fun c p ->
  ManualReifiers.pair_reifier ManualReifiers.string_reifier glam_reifier c p

let env_reifier c xs =
  List.reifier pair_reifier c xs *)
let show_nat_list   = GT.(show List.ground @@ show Nat.ground)
let show_natl_listl = GT.(show List.logic  @@ show Nat.logic)

let runN n = runR Nat.reifier show_nat (GT.show(Nat.logic)) n
let runL n = runR (List.reifier Nat.reifier) show_nat_list show_natl_listl n

let _freeVars =
  runN         3   qr qrh (REPR (fun q r   -> Nat.mulo q   r   q             ));
  runL      (-1)    q  qh (REPR (fun q     -> List.lengtho q ?$3             ));
  ()
