open Printf
open MiniKanren
open Tester.M
open ImplicitPrinters
open MiniHelpers

let (!) = inj

let good_cname name = (name === !"A") ||| (name === !"B")

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

implicit module Show_value : (SHOW with type t = Value.t) = struct
  type t = Value.t
  let show what =
    let b = Buffer.create 10 in
    let fmt = Format.formatter_of_buffer b in
    Value.print fmt what;
    Format.pp_print_flush fmt ();
    Buffer.contents b
end

let empty_value name = !(Value.VC (!name, llist_nil))
let make_value name xs = (Value.VC (!name, of_list xs))
let make_int n = (Value.VInt !n)

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
let make_int_pat n = Pat.PInt (!n)


module Subst = struct
  type t = Bottom | Smth of ((string logic) * Value.t logic ) llist logic

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

let subs_uniono_helper s1 s2 ans st =
  print_endline "uniono helper";
  subs_uniono s1 s2 ans st

let rec my_combine (xs:Value.t llist logic) (ys: Pat.t llist logic) ans =
  fresh (hx tx hy ty temp)
        (list_cons xs hx tx)
        (list_cons ys hy ty)
        (my_combine tx ty temp)
        (ans === !(hx,hy) % temp)

let rec folder (acc: Subst.t logic) (x: (Value.t logic * Pat.t logic) logic) ans =
    conde
      [ (acc === bottom) &&& (ans === bottom)
      ; fresh (v p ans0)
              (x === !(v,p))
              (evalo v !<p ans0)
              (conde
                 [ (ans0 === bottom) &&& (ans === bottom)
                 ; (ans0 =/= bottom) &&& (subs_uniono_helper ans0 acc ans)
                 ])]

and evalo what patts ans =
  let open Value in
  let open Pat in

  conde
    [ (patts === llist_nil) &&& (ans === bottom)
    ; fresh (p1 pothers)
            (list_cons patts p1 pothers)
            (conde
               [ (p1 === !PAny) &&& (ans === subs_empty)
               ; fresh (name)
                       (p1 === !(PVar name))
                       (subs_make name what ans)
               ; fresh (n m)
                       (what === !(VInt n))
                       (p1 === !(PInt m))
                       (conde [ (n===m) &&& (ans === subs_empty)
                              ; (n=/=m) &&& (ans === bottom)
                              ])

               ; (evalo_const what p1 ans)
               ; fresh (name ps vs pairs rez)
                       (p1 === !(PC (name,ps)) )
                       (what === !(VC (name,vs)) )
                       (conde
                          [ (ps === llist_nil) &&& (vs=== llist_nil) &&& (ans === subs_empty)
                          ; fresh (_X)
                              (my_combine vs ps pairs)
                              (foldo folder subs_empty pairs rez)
                              (conde
                                 [ (rez === bottom) &&& (evalo what pothers ans)
                                 ; (rez =/= bottom) &&& (ans === rez)
                                 ])
                          ])
               ])
    ]
and evalo_const what patt ans =
  let open Value in
  let open Pat in
  fresh (n m)
    (what === !(VInt n))
    (patt === !(PInt m))
    (conde [ (n===m) &&& (ans === subs_empty)
           ; (n=/=m) &&& (ans === bottom)
           ])


let wat1 what ans =
  let open Value in
  fresh (n)
    (what === !(VInt n))
    (* (ans === what) *)

let make_empty_constr name = Pat.PC(!name, llist_nil)

let _ =
  let open Tester.M.ConvenienceCurried in
  let wrap ?(n=1) what patts =
    Tester.run1 ~n (REPR(evalo what patts ))
  in


  let open Value in
  let open Pat in
  wrap (empty_value "A")            (of_list [ PAny;      make_empty_constr "xx" ]);
  wrap (empty_value "B")            (of_list [ PVar !"x"; make_empty_constr "B" ]);
  wrap (empty_value "A")            (of_list [ make_empty_constr "A" ]);
  Tester.run1 ~n:1 (REPR(evalo
                           !(make_value  "A" [make_value "B" []])
                           (of_list [make_pat  "A" [make_pat "B" []] ])
                        ));
  ;
  Tester.run1 ~n:1 (REPR(evalo
                           !(make_value  "A" [])
                           (of_list [make_pat  "A" [] ])
                        ));
  ;
  Tester.run1 ~n:1 (REPR(evalo
                           !(make_int 5)
                           (of_list [make_pvar "a"])
                        ));
  ;
  Tester.run1 ~n:1 (REPR(evalo !(make_int 5) (of_list [make_int_pat 5]) ));
  Tester.run1 ~n:1 (REPR(evalo !(make_int 5) (of_list [make_int_pat 6]) ));

  Tester.run1 ~n:1 (REPR(fun q -> evalo_const q !(make_int_pat 5) bottom));
  Tester.run1 ~n:1 (REPR(fun q -> evalo_const q !(make_int_pat 5) subs_empty));
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
