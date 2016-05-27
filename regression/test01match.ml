open Printf
open MiniKanren
open Tester.M
open ImplicitPrinters
open MiniHelpers

let (!) = inj

module Value = struct
  type t = V0 | V1 of t llist logic | V2 of t llist logic

  open Format

  let print ppf root =
    let rec helper = function
      | V0 -> fprintf ppf "V0"
      | V1 xs when llist_is_empty_logic xs -> fprintf ppf "V1"
      | V1 xs -> fprintf ppf "V1 (%a)" fprintf_logic xs
      | V2 xs when llist_is_empty_logic xs -> fprintf ppf "V2"
      | V2 xs -> fprintf ppf "V2 (%a)" fprintf_logic xs
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

module Pat = struct
  type t =
    | Pany
    | Pvar of string logic
    | PC0
    | PC1 of t llist logic | PC2 of t llist logic

  open Format

  let print ppf root =
    let rec helper = function
      | Pany -> fprintf ppf "_"
      | Pvar s -> fprintf ppf "\"%a\"" fprintf_logic s
      | PC0 -> fprintf ppf "PC0"
      | PC1 xs when llist_is_empty_logic xs -> fprintf ppf "PC1"
      | PC1 xs -> fprintf ppf "PC1 (%a)" fprintf_logic xs
      | PC2 xs when llist_is_empty_logic xs -> fprintf ppf "PC2"
      | PC2 xs -> fprintf ppf "PC2 (%a)" fprintf_logic xs
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

let extract_args_V1 what ans = what === !(Value.V1 ans)

open Pat

let rec my_combine (xs:Value.t llist logic) (ys: Pat.t llist logic) ans =
  fresh (hx tx hy ty temp)
        (list_cons xs hx tx)
        (list_cons ys hy ty)
        (my_combine tx ty temp)
        (ans === !(hx,hy) % temp)

let rec evalo what patts ans =
  let open Value in
  let folder (acc: Subst.t logic) (x: (Value.t logic * Pat.t logic) logic) ans =
    (ans === acc)
  in

  conde
    [ (patts === llist_nil) &&& (ans === bottom)
    ; fresh (p1 pothers)
            (list_cons patts p1 pothers)
            (conde
               [ (p1 === !Pany) &&& (ans === !Subst.empty)
               ; fresh (name)
                       (p1 === !(Pvar name))
                       (subs_make name what ans)
               ; (p1 === !PC0) &&&
                   (conde
                      [ (what === !V0) &&& (ans === !Subst.empty)
                      ; (what =/= !V0) &&& (ans === bottom)
                      ])
               ; (p1 === !(PC1 llist_nil)) &&&
                   (conde
                      [ (what === !(V1 llist_nil)) &&& (ans === !Subst.empty)
                      ; (what =/= !(V1 llist_nil)) &&& (ans === bottom)
                      ])


               ; fresh (ps vs pairs rez)
                       (p1 === !(PC1 ps))
                       (what === !(Value.V1 vs))
                       (my_combine vs ps pairs)
                       (foldo folder !Subst.empty pairs rez)
                       (conde
                          [ (rez === bottom) &&& (evalo what pothers ans)
                          ; (rez =/= bottom) &&& (ans === rez)
                          ])


               ])
    ]


(*

 let check_pats: patts list -> Value.t option


 *)

let _ =
  let open Tester.M.ConvenienceCurried in
  let wrap ?(n=1) what patts =
    Tester.run1 ~n (REPR(evalo what patts ))
  in

  let wrap2 ?(n=1) patts =
    Tester.run1 ~n (REPR(fun q -> evalo q patts bottom ))
  in

  let open Value in
  wrap !V0               (of_list [ Pany; PC0 ]);
  wrap !V0               (of_list [ Pvar !"x"; PC0 ]);
  wrap !(V1 (of_list[])) (of_list [ PC0 ]);
  wrap2 (of_list [ Pvar !"x"; PC0 ]);
  wrap2 (of_list [ PC0 ]);
  ()



let goal1 x = (x === !5)

let ___ () =
  let bad_example =
    let open Tester.M.ConvenienceCurried in
    run one goal1
       (fun reif ->
        match reif 1 with
        | [] -> None
        | (_log,(ans,_)) :: _ -> Some ans
       )
  in
  match bad_example with
  | None -> print_endline "none"
  | Some x -> printf "%s\n%!" (show_logic_naive x)
