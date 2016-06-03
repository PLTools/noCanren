open Printf
open MiniKanren
open Tester.M
open ImplicitPrinters
open MiniHelpers

let (!) = inj

(* let good_cname name = (name === !"A") ||| (name === !"B") *)

module Value = struct
  type t = VC of string logic * t llist logic
         | VInt of int logic

  open Format

  let print ppf root =
    let prl = fprintf_logic_with_cs in
    let rec helper = function
      | VC (name,xs) when llist_is_empty_logic xs -> fprintf ppf "%a" prl name
      | VC (name,xs) -> fprintf ppf "%a (%a)" prl name prl xs
      | VInt n -> fprintf ppf "VInt %a" prl n
    in
    helper root
end

let print_as_format print what =
  let b = Buffer.create 10 in
  let fmt = Format.formatter_of_buffer b in
  print fmt what;
  Format.pp_print_flush fmt ();
  Buffer.contents b

implicit module Show_value : (SHOW with type t = Value.t) = struct
  type t = Value.t
  let show what = print_as_format Value.print what
end

let empty_value name = !(Value.VC (!name, llist_nil))
let make_value name xs = (Value.VC (!name, of_list xs))
let make_vint n = Value.VInt !n

module Pat = struct
  type t =
    | PAny
    | PVar of string logic
    | PInt of int logic
    | PC of string logic * t llist logic

  open Format

  let print ppf root =
    let rec helper = function
      | PAny -> fprintf ppf "_"
      | PVar s -> fprintf ppf "\"%a\"" fprintf_logic s
      | PC (name,xs) when llist_is_empty_logic xs -> fprintf ppf "%a" fprintf_logic name
      | PC (name,xs) -> fprintf ppf "%a (%a)" fprintf_logic name fprintf_logic xs
      | PInt n -> fprintf ppf "%a" fprintf_logic n
    in
    helper root
end

implicit module Show_pat : (SHOW with type t = Pat.t) = struct
  type t = Pat.t
  let show what =
    let b = Buffer.create 10 in
    let fmt = Format.formatter_of_buffer b in
    Pat.print fmt what;
    Format.pp_print_flush fmt ();
    Buffer.contents b
end

let make_pat name args = Pat.PC (!name, of_list args)
let make_pvar name = Pat.PVar !name
let make_pint n = Pat.PInt !n
let make_empty_constr name = Pat.PC(!name, llist_nil)

module Subst = struct
  type lst = ((string logic) * Value.t logic ) llist
  type t = Bottom | Smth of lst logic

  let print ppf root =
    let open Format in
    let rec helper = function
      | Bottom -> fprintf ppf "_|_"
      | Smth l -> fprintf ppf "%a" fprintf_logic l
    in
    helper root

  let empty : t = Smth llist_nil
end

implicit module Show_subst : (SHOW with type t = Subst.t) =
struct
  type t = Subst.t
  let show what =
    let b = Buffer.create 10 in
    let fmt = Format.formatter_of_buffer b in
    Subst.print fmt what;
    Format.pp_print_flush fmt ();
    Buffer.contents b
end

let bottom = !Subst.Bottom
let subs_make (name: string logic) (what: Value.t logic) (ans: Subst.t logic) =
  ans === !(Subst.Smth ( !(name,what) % llist_nil))

let subs_empty = !(Subst.Smth llist_nil)

let subs_assoco name subs (ans: Value.t logic option logic) =
  let open Subst in

  let rec helper (xs: Subst.lst logic) ans =
    conde [ (xs === llist_nil) &&& (ans === !None)
          ; fresh (h tl name2 v2)
                  (list_cons xs h tl)
                  (h === !(name2, v2))
                  (conde [ (name===name2) &&& (ans === !(Some v2))
                         ; (name=/=name2) &&& (helper tl ans)
                         ])
          ]
  in

  conde
    [ (subs === bottom) &&& (ans === !None)
    ; fresh (xs s1 v1)
        (subs === !(Smth xs))
        (helper xs ans)
    ]

module Expr = struct
  type t =
    | EEQ  of t logic * t logic (* -> int t *)
    | ENEQ of t logic * t logic (* -> int t *)
    | EVar of string logic (* -> int t *)
    | EInt of int logic (* -> int t *)
    (* | EBool: bool logic -> int t *)
    (* | EGET : Value.t logic * Value.t logic -> bool t (\* greater or equal *\) *)
    (* | ELT  : Value.t logic * Value.t logic -> bool t (\* less than *\) *)

  let print : Format.formatter -> t -> unit = fun ppf root ->
    let open Format in
    let prl = fprintf_logic_with_cs in
    let rec helper : t -> unit = function
      | EEQ  (l,r) -> fprintf ppf "(%a=%a)" prl l prl r
      | ENEQ (l,r) -> fprintf ppf "(%a<>%a)"  prl l prl r
      | EVar x     -> fprintf ppf "(EVar %a)" prl x
      | EInt n     -> fprintf ppf "(EInt  %a)" prl n
      (* | EBool b    -> fprintf ppf "(EBool %a)" prl b *)
      (* | EGET (l,r) -> fprintf ppf "(%a>=%a)" prl l prl r *)
      (* | ELT  (l,r) -> fprintf ppf "(%a<%a)"  prl l prl r *)
    in
    helper root
end

implicit module Show_expr (* {X:SHOW} *) : (SHOW with type t = (* X.t *) Expr.t) = struct
  type t = (* X.t *) Expr.t
  let show what = print_as_format Expr.print what
end

(* let make_ge l r = Expr.(EGET (l,r)) *)
(* let make_lt l r = Expr.(ELT  (l,r)) *)
let make_eeq  l r = Expr.(EEQ  (l,r))
let make_eneq l r = Expr.(ENEQ (l,r))
let make_evar name = Expr.(EVar !name)
let make_eint n = Expr.(EInt !n)
let etrue  = !Expr.(EInt !1)
let efalse = !Expr.(EInt !0)

let rec evalo_expr : (Subst.t logic) -> Expr.t logic -> (Value.t) logic -> goal =
  fun subs e ans ->
  let open Expr in
  let open Value in
  conde
    [ fresh (n)
            (e === !(EInt n))
            (ans === !(VInt n))
    ; fresh (name maybe_ans)
            (e === !(EVar name))
            (subs_assoco name subs maybe_ans)
            (maybe_ans === !(Some ans))
    ; fresh (l r ansl ansr)
            (e === !(EEQ (l,r)) )
            (evalo_expr subs l ansl)
            (evalo_expr subs r ansr)
            (conde
               [ fresh (m n)
                       (ansl === !(VInt m))
                       (ansr === !(VInt n))
                       (conde
                          [ (m===n) &&& (ans === !(VInt !1))
                          ; (m=/=n) &&& (ans === !(VInt !0))
                          ])
               ])
    ; fresh (l r ansl ansr)
            (e === !(ENEQ (l,r)) )
            (evalo_expr subs l ansl)
            (evalo_expr subs r ansr)
            (conde
               [ fresh (m n)
                       (ansl === !(VInt m))
                       (ansr === !(VInt n))
                       (conde
                          [ (m===n) &&& (ans === !(VInt !0))
                          ; (m=/=n) &&& (ans === !(VInt !1))
                          ])
               ])
    ]

let subs_extendo name what base ans =
  let open Subst in
  conde
    [ (base === !Bottom) &&& (subs_make name what ans)
    ; fresh (xs temp)
            (base === !(Smth xs))
            (temp === !(name,what) % xs )
            (ans  === !(Smth temp) )
    ]

let extract_args_V1 what name args = what === !(Value.VC (name,args))

let subs_uniono (s1: Subst.t logic) (s2: Subst.t logic) ans =
  let open Subst in
  conde
    [ (s1 === bottom) &&& (ans === bottom)
    ; (s2 === bottom) &&& (ans === bottom)
    ; fresh (xs ys ans0)
            (s1 === !(Smth xs))
            (s2 === !(Smth ys))
            (appendo xs ys ans0)
            (ans === !(Smth ans0))
    ]

let rec my_combine (xs:Value.t llist logic) (ys: Pat.t llist logic) ans =
  fresh (hx tx hy ty temp)
        (list_cons xs hx tx)
        (list_cons ys hy ty)
        (my_combine tx ty temp)
        (ans === !(hx,hy) % temp)


(* let rec wtf_term_check (ps: int llist logic) *)
(*                        (ans: (int logic * int list option) logic) = *)
(*   fresh (h tl) *)
(*         (ps === h%tl) *)
(*         (ans === !(h, (None: int list option)) ) *)

type guard = Expr.t logic option

(* assigns None for every pattern. Useful to construct arguments when match two constrs. *)
let rec noguard (ps: Pat.t llist logic) (ans: guard llist logic) =
  conde
    [ (ps===llist_nil) &&& (ans === llist_nil)
    ; call_fresh (fun (h: Pat.t logic) ->
          fresh (tl ans2 wtf)
            (list_cons ps h tl)
            (noguard tl ans2)
            (wtf === !(None: guard))
            (ans === wtf % ans2)
        )
    ]

let rec folder (acc: Subst.t logic) (x: (Value.t logic * Pat.t logic) logic) ans =
    conde
      [ (acc === bottom) &&& (ans === bottom)
      ; fresh (v p ans0 guards)
              (x === !(v,p))
              (guards === !< (!None) )
              (evalo v !<p guards ans0)
              (conde
                 [ (ans0 === bottom) &&& (ans === bottom)
                 ; (ans0 =/= bottom) &&& (subs_uniono ans0 acc ans)
                 ])]

and with_guard maybe_g subs ans =
  let open Value in
  conde
    [ fresh (g grez)
        (maybe_g === !(Some g))
        (evalo_expr subs g grez)
        (conde
           [ (grez === !(VInt !1)) &&& (ans === subs)
           ; (grez === !(VInt !0)) &&& (ans === bottom)
           ])
    ; (maybe_g === !None) &&& (ans === subs)
    ]

(* Main function. *)
and evalo what patts (guards: guard llist logic) ans =
  conde
    [ (patts === llist_nil) &&& (guards === llist_nil) &&& (ans === bottom)
    ; fresh (ph ptl gh gtl rez)
            (list_cons patts ph ptl)
            (list_cons guards gh gtl)
            (evalo_pat subs_empty what ph gh rez)
            (conde
               [ (rez === bottom) &&& (evalo what ptl gtl ans)
               ; (rez =/= bottom) &&& (ans === rez)
               ])
    ]

and evalo_pat subs what pat (guard: Expr.t logic option logic) ans =
  let open Value in
  let open Pat in

  (conde
     [ (pat === !PAny) &&& (with_guard guard subs ans)

     ; fresh (name temp_subs)
         (pat === !(PVar name))
         (subs_make name what temp_subs)
         (with_guard guard temp_subs ans)
     ; fresh (n m)
         (what === !(VInt n))
         (pat === !(PInt m))
         (conde [ (n===m) &&& (with_guard guard subs_empty ans)
                ; fresh (temp_ans)
                        (n=/=m)
                        (* Not sure that next conde is really required *)
                        (conde
                           [ (with_guard guard subs_empty bottom) &&& (ans === bottom)
                           ; (with_guard guard subs_empty temp_ans) &&&
                               (temp_ans =/= bottom) &&& (ans === bottom)
                           ])


                ])

     ; (evalo_const subs what pat guard ans)

     (* match two constructors *)
     ; fresh (name ps vs pairs rez)
         (pat === !(PC (name,ps)) )
         (what === !(VC (name,vs)) )
         (conde
            [ (ps === llist_nil) &&& (vs=== llist_nil) &&& (ans === subs_empty)
            ; fresh (gs)
                (noguard ps gs)
                (my_combine vs ps pairs)
                (foldo folder subs pairs rez)
                (conde
                   [ (rez === bottom) &&& (ans===bottom)
                   ; (rez =/= bottom) &&& (with_guard guard rez ans)
                   ])
            ])
     (* TODO: Maybe check that we match VInt and PConstr and return bottom in that case *)
     ])

and evalo_const subs what patt guard ans =
  let open Value in
  let open Pat in
  fresh (n m)
    (what === !(VInt n))
    (patt === !(PInt m))
    (conde [ fresh (grez)
                   (n===m)
                   (with_guard guard subs_empty ans)
                   (* (conde *)
                   (*    [ (guard === !None) &&& (ans === subs) *)
                   (*    ; fresh (g) *)
                   (*            (guard === !(Some g)) *)
                   (*            (evalo_expr subs g grez) *)
                   (*            (conde *)
                   (*               [ (grez === !(VInt !1)) &&& (ans === subs) *)
                   (*               ; (grez =/= !(VInt !1)) &&& (ans === bottom) *)
                   (*               ]) *)
                   (*    ]) *)
           ; (n=/=m) &&& (ans === bottom)
           ])

let _ =
  let open Value in
  let open Pat in
  let run1 = Tester.run1 in

  (* wrap (empty_value "A")            (of_list [ PAny;      make_empty_constr "xx" ]); *)
  (* wrap (empty_value "B")            (of_list [ PVar !"x"; make_empty_constr "B" ]); *)
  (* wrap (empty_value "A")            (of_list [ make_empty_constr "A" ]); *)
  (* run1 ~n:1 (REPR(evalo *)
  (*                          !(make_value  "A" [make_value "B" []]) *)
  (*                          (of_list [make_pat  "A" [make_pat "B" []] ]) *)
  (*                       )); *)
  (* run1 ~n:1 (REPR(evalo *)
  (*                          !(make_value  "A" []) *)
  (*                          (of_list [make_pat  "A" [] ]) *)
  (*                       )); *)
  (* run1 ~n:1 (REPR(evalo *)
  (*                          !(make_vint 5) *)
  (*                          (of_list [make_pvar "a"]) *)
  (*                       )); *)

  let single_no_guard = of_list [(None : Expr.t logic option)] in
  run1 ~n:1 (REPR(evalo !(make_vint 5) (of_list [make_pint 5])
                    single_no_guard
                 ));
  run1 ~n:1 (REPR(evalo !(make_vint 5) (of_list [make_pint 6])
                    single_no_guard
                 ));

  run1 ~n:1 (REPR(fun q -> evalo_const subs_empty q
                                       !(make_pint 5)
                                       !(None: Expr.t logic option)
                                       bottom ));
  run1 ~n:1 (REPR(fun q -> evalo_const subs_empty q
                                       !(make_pint 5)
                                       !(None: Expr.t logic option)
                                       subs_empty));

  run1 ~n:1 (REPR(fun q -> evalo !(make_vint 1)
                                 (of_list [ (make_pvar "x")] )
                                 (of_list [ (Some !(make_eeq !(make_evar "x")
                                                             !(make_eint 2)) )
                                          ])
                                 q));

  run1 ~n:1 (REPR(fun q -> evalo !(make_vint 1)
                                 (of_list [ (make_pvar "x")] )
                                 (of_list [ (Some !(make_eeq !(make_evar "x")
                                                             !(make_eint 1)) )
                                          ])
                                 q));

  (* really exhaustive. Doesn't return any answers *)
  run1 ~n:1 (REPR(fun q -> evalo q
                                 (of_list [ make_pvar "x"
                                          ; make_pvar "x"
                                          ])
                                 (of_list [ Some !(make_eeq  !(make_evar "x")
                                                             !(make_eint 1))
                                          ; Some !(make_eneq !(make_evar "x")
                                                             !(make_eint 1))
                                          ])
                                 bottom));

  (* should be not exhaustive. Returns some answers *)
  run1 ~n:1 (REPR(fun q -> evalo q
                                 (of_list [ make_pvar "x"
                                          ])
                                 (of_list [ Some !(make_eeq  !(make_evar "x")
                                                             !(make_eint 1))
                                          ])
                                 bottom));

  (* run1 ~n:1 (REPR(subs_assoco !"abc" bottom)); *)
  (* run1 ~n:1 (REPR(subs_assoco !"abc" !(Subst.Smth (of_list [ (!"x", !(make_vint 5))])) )); *)
  (* run1 ~n:1 (REPR(subs_assoco !"x"   !(Subst.Smth (of_list [ (!"x", !(make_vint 5))])) )); *)
  (* run1 ~n:1 (REPR(evalo_expr  !(Subst.Smth (of_list [ (!"x", !(make_vint 5))])) *)
  (*                             !(make_evar "x") *)
  (*                )); *)
  (* run1 ~n:1 (REPR(evalo_expr  !(Subst.Smth (of_list [ (!"x", !(make_vint 5))])) *)
  (*                             !(make_eint 6) *)
  (*                )); *)
  (* run1 ~n:1 (REPR(evalo_expr  !(Subst.Smth (of_list [ (!"x", !(make_vint 5))])) *)
  (*                             !(make_eeq !(make_eint 1) !(make_eint 1)) *)
  (*                )); *)
  (* run1 ~n:1 (REPR(evalo_expr  !(Subst.Smth (of_list [ (!"x", !(make_vint 5))])) *)
  (*                             !(make_eeq !(make_eint 1) !(make_eint 2)) *)
  (*                )); *)
  (* run1 ~n:1 (REPR(evalo_expr  !(Subst.Smth (of_list [ (!"x", !(make_vint 5))])) *)
  (*                             !(make_eeq !(make_evar "x") !(make_eint 5)) *)
  (*                )); *)



  (* Tester.run2 ~n:3 (REPR(wat1)); *)

  (* Tester.run1 ~n:1 (REPR(fun q -> evalo q (of_list [ Pat.PVar !"x"; make_empty_constr "A" ]) bottom)); *)
  (* Tester.run1 ~n:1 (REPR(fun q -> evalo q (of_list [ make_empty_constr "A" ]) bottom)); *)
  (* Tester.run1 ~n:1 (REPR(fun q -> folder !Subst.empty  !( !(make_value "A" []), !(make_empty_constr "A")) q)); *)
  (* Tester.run1 ~n:1 (REPR(fun q -> folder !Subst.empty  !( !(make_value "A" [make_value "C" []]), !(make_pat "A" [make_pat "C" []])) q)); *)
  (* Tester.run2 ~n:1 (REPR(fun q v -> q =/= v)); *)
  ()



let goal1 x = (x === !5)

let ___ () =
  let bad_example =
    let open Tester.M.ConvenienceCurried in
    run one goal1
       (fun reif ->
        match Stream.take ~n:1 reif  with
        | [] -> None
        | (_log,ans) :: _ -> Some ans
       )
  in
  match bad_example with
  | None -> print_endline "none"
  | Some x ->
     printf_logic_with_cs x;
     printf "\n%!"

(* let () = *)
(*   Tester.run1 ~n:1 (REPR(fun q -> (q=/= !1)) ); *)
(*   () *)
