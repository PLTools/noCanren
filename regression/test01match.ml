open Printf
open MiniKanren
open Tester.M
open ImplicitPrinters
open MiniHelpers

let (!) = inj

let good_cname name = (name === !"A") ||| (name === !"B")

module Value = struct
  type t = VC of string logic * t llist logic

  open Format

  let print ppf root =
    let rec helper = function
      | VC (name,xs) when llist_is_empty_logic xs -> fprintf ppf "%a" fprintf_logic name
      | VC (name,xs) -> fprintf ppf "%a (%a)" fprintf_logic name fprintf_logic xs
      (* | V0 -> fprintf ppf "V0" *)
      (* | V1 xs when llist_is_empty_logic xs -> fprintf ppf "V1" *)
      (* | V1 xs -> fprintf ppf "V1 (%a)" fprintf_logic xs *)
      (* | V2 xs when llist_is_empty_logic xs -> fprintf ppf "V2" *)
      (* | V2 xs -> fprintf ppf "V2 (%a)" fprintf_logic xs *)
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

module Pat = struct
  type t =
    | Pany
    | Pvar of string logic
    | PC of string logic * t llist logic
    (* | PC1 of t llist logic | PC2 of t llist logic *)

  open Format

  let print ppf root =
    let rec helper = function
      | Pany -> fprintf ppf "_"
      | Pvar s -> fprintf ppf "\"%a\"" fprintf_logic s
      | PC (name,xs) when llist_is_empty_logic xs -> fprintf ppf "%a" fprintf_logic name
      | PC (name,xs) -> fprintf ppf "%a (%a)" fprintf_logic name fprintf_logic xs

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
               [ (p1 === !Pany) &&& (ans === !Subst.empty)
               ; fresh (name)
                       (p1 === !(Pvar name))
                       (subs_make name what ans)
(*               ; (p1 === !PC0) &&&
                   (conde
                      [ (what === !V0) &&& (ans === !Subst.empty)
                      ; (what =/= !V0) &&& (ans === bottom)
                      ])
               ; (p1 === !(PC1 llist_nil)) &&&
                   (conde
                      [ (what === !(V1 llist_nil)) &&& (ans === !Subst.empty)
                      ; (what =/= !(V1 llist_nil)) &&& (ans === bottom)
                      ]) *)


               ; fresh (name ps vs pairs rez)
                       (p1 === !(PC (name,ps)) )
                       (what === !(VC (name,vs)) )
                       (conde
                          [ (ps === llist_nil) &&& (vs=== llist_nil) &&& (ans === !Subst.empty)
                          ; fresh (_X)
                              (my_combine vs ps pairs)
                              (foldo folder !Subst.empty pairs rez)
                              (conde
                                 [ (rez === bottom) &&& (evalo what pothers ans)
                                 ; (rez =/= bottom) &&& (ans === rez)
                                 ])
                          ])


               ])
    ]

let make_empty_constr name = Pat.PC(!name, llist_nil)

let _ =
  let open Tester.M.ConvenienceCurried in
  let wrap ?(n=1) what patts =
    Tester.run1 ~n (REPR(evalo what patts ))
  in

  let wrap2 ?(n=1) patts =
    Tester.run1 ~n (REPR(fun q -> evalo q patts bottom ))
  in

  let open Value in
  let open Pat in
  wrap (empty_value "A")            (of_list [ Pany;      make_empty_constr "xx" ]);
  wrap (empty_value "B")            (of_list [ Pvar !"x"; make_empty_constr "B" ]);
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
  (* Tester.run1 ~n:1 (REPR(fun q -> evalo q (of_list [ Pat.Pvar !"x"; make_empty_constr "A" ]) bottom)); *)
  (* Tester.run1 ~n:1 (REPR(fun q -> evalo q (of_list [ make_empty_constr "A" ]) bottom)); *)
  (* Tester.run1 ~n:1 (REPR(fun q -> folder !Subst.empty  !( !(make_value "A" []), !(make_empty_constr "A")) q)); *)
  (* Tester.run1 ~n:1 (REPR(fun q -> folder !Subst.empty  !( !(make_value "A" [make_value "C" []]), !(make_pat "A" [make_pat "C" []])) q)); *)
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
