(* to get source:
  mkae ppx && ocamlfind ppx_tools/rewriter "`ocamlfind query ppx_deriving`/ppx_deriving -deriving-plugin _build/ppx/ppx_deriving_gt.cma" regression/test100ppxgt.ml
*)
type token_env = int
(* let () = print_endline @@ GT.show token_env 5 *)

type 'a logic =
| Var   of GT.int * 'a logic GT.list  (* * 'a logic *)
| Value of 'a
[@@deriving gt {show} ]

let logic = {GT.gcata = (); plugins =
  object
    (* method gmap    = logic.plugins#gmap *)
    method show fa x =
       GT.transform(logic) (GT.lift fa)
          (object inherit ['a] show_logic_t
            method c_Var _ s i cs =
              let c = match cs with
              | [] -> ""
              | _  -> Printf.sprintf " %s" (GT.show(GT.list) (fun l -> "=/= " ^ s.GT.f () l) cs)
              in
              Printf.sprintf "_.%d%s" i c
            method c_Value _ _ x = x.GT.fx ()
           end)
          ()
          x
  end
}

let () =
  let q = Var (1,[]) in
  let r = Value "s" in
  let s = Var (2,[q;r]) in
  let open GT in
  List.iter (fun l -> print_endline @@ (show logic) (show string) l) [q;r;s]
(*
type ('a, 'l) llist = Nil | Cons of 'a * 'l
[@@deriving gt {show} ]

let rec show_llist arg xs = GT.(show llist arg (show_llist arg)) xs

let () =
  let q = Nil in
  let r = Cons ("s", Nil) in
  let s = Cons ("a", Cons ("b", Nil)) in
  let open GT in
  List.iter (fun xs -> print_endline @@ show_llist (show string) xs) [q;r;s]
*)
(*
type 'a lnat = O | S of 'a
[@@deriving gt { show } ]

let () =
  let q = O in
  let r = S O in
  let s = S (S O) in
  let rec show_nat n = GT.(show lnat show_nat) n in
  let open GT in
  List.iter (fun n -> print_endline @@ show_nat n) [q;r;s] *)
