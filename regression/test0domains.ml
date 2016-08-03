open Printf
open MiniKanren
open Tester.M
open ImplicitPrinters

let (!) = inj

let pp_logic _ fmt logic = fprintf_logic fmt logic

type person =
  | Scot | Irishman | Welshman | Englishman
  | Unknown of string logic  [@@deriving show]

(* Can be easily hidden into @@deriving *)
implicit module Show_person : (SHOW with type t = person) = struct
  type t = person
  let show = show_person
end


module type DOMAIN_FOR_VAR = sig
  type t
  val domain : t logic -> goal
end

(* Can be easily hidden into @@deriving too *)
implicit module Domain_person : (DOMAIN_FOR_VAR with type t = person) = struct
  type t = person
  let domain q =
    conde
      [ (q === !Irishman)
      ; (q === !Welshman)
      ; (q === !Englishman)
      ; (q === !Scot)
      ; fresh s (q === !(Unknown s))
      ]
end

let (=//=) {D: DOMAIN_FOR_VAR} (l: D.t logic) (r: D.t logic) =
  (D.domain l) &&& (D.domain r) &&& (l =/= r)

let nation1 q = (q =/=  !Irishman) &&& (q =/=  !Welshman) &&& (q =/=  !Englishman)
let nation2 q = (q =//= !Irishman) &&& (q =//= !Welshman) &&& (q =//= !Englishman)

let _ =
  let open Tester in
  run1 ~n:2 (REPR(nation1));
  run1 ~n:2 (REPR(nation2));
  ()
(*

`nation1`, 2 answers {
q=_.10 {{=/= Test0domains.Englishman Test0domains.Welshman Test0domains.Irishman}};
}
`nation2`, 2 answers {
q=Test0domains.Scot;
q=(Test0domains.Unknown _.13);
}

*)
