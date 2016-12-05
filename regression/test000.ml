open GT
open MiniKanren
open Tester
open Printf

(* let just_a a = a === (5 |> lift |> inj) *)

(* let rec show_list l = show(llist) (show(int)) show_list l;; *)
(*
@type 'a test = A of 'a with show;;

module LTest = Fmap (struct type 'a t = 'a test let fmap f = function A x -> A (f x)  end)
module LOption = Fmap (struct
  type 'a t = 'a option
  let fmap f = function Some x -> Some(f x) | None -> None
end)

let _ =
  let () = MiniKanren.run q
    (fun q -> inj (LTest.fmap (A q)) === inj (LTest.fmap (A (inj (lift 5)))))
    (fun qs -> printf "%s\n" (show(int) @@ Stream.hd qs))
  in

  let (_: (int, int logic) fancy) = inj @@ lift 5 in
  let (_: (int option, int logic option) fancy) = LOption.fmap (Some (inj @@ lift 5)) in
  let () =
    MiniKanren.run q
      (fun q -> q === inj (LOption.fmap (Some (inj (lift 5)))))
      (fun qs -> printf "%s\n" (show(option) (show(int)) @@ Stream.hd qs))
  in *)
(*
  run show_int_list  1  q (REPR (fun q   -> appendo q (inj_list [3; 4]) (inj_list [1; 2; 3; 4]))) qh;
  run show_int_list  4 qr (REPR (fun q r -> appendo q (inj_list []) r                          )) qrh;
  run show_int_list  1  q (REPR (fun q   -> reverso q (inj_list [1; 2; 3; 4])                  )) qh;
  run show_int_list  1  q (REPR (fun q   -> reverso (inj_list []) (inj_list [])                )) qh;
  run show_int_list  1  q (REPR (fun q   -> reverso (inj_list [1; 2; 3; 4]) q                  )) qh;
  run show_int_list  1  q (REPR (fun q   -> reverso q q                                        )) qh;
  run show_int_list  2  q (REPR (fun q   -> reverso q q                                        )) qh;
  run show_int_list  3  q (REPR (fun q   -> reverso q q                                        )) qh;
  run show_int_list 10  q (REPR (fun q   -> reverso q q                                        )) qh;
  run show_int_list  2  q (REPR (fun q   -> reverso q (inj_list [1])                           )) qh;
  run show_int_list  1  q (REPR (fun q   -> reverso (inj_list [1]) q                           )) qh;
  run show_int       1  q (REPR (fun q   -> a_and_b q                                          )) qh;
  run show_int       2  q (REPR (fun q   -> a_and_b' q                                         )) qh;
  run show_int      10  q (REPR (fun q   -> fives q                                            )) qh
*)
  (* ()
;; *)

@type 'a maybe =
  | Nothing
  | Just of 'a  with show;;
module Maybe = Fmap1 (struct
  type 'a t = 'a maybe
  let fmap f x =
    let () = printf "Maybe.fmap of '%s'\n%!" (generic_show x) in
    match x with
    | Just a -> Just (f a)
    | Nothing -> Nothing
end)

(* let (_: int) = (inj @@ Maybe.fmap @@ (Just (inj@@lift 15)) ) *)
(* let (_:int) = (===)
let (_:_ fancy -> goal) = fun q ->
  let (right: (int maybe, int logic maybe logic,
               int inner_logic maybe inner_logic) fancy)
    = inj @@ Maybe.fmap @@ (Just (inj@@lift 15))  in
  q === right
let (_:int) = MiniKanren.run q *)

let show_logic_maybe f x : string =
  show inner_logic (show(maybe) f) x

(* let () =
  MiniKanren.run q
    (fun q ->
      let () = printf "+++++++++++++++++++++++++++\n%!" in
      let (right: (int, int logic , int inner_logic ) fancy)  = inj@@lift 15 in
      q === right)
    (fun qs -> printf "%s\n" ((show(inner_logic)@@show(int)) @@ Stream.hd qs))
;; *)

let () =
  MiniKanren.run q
    (fun q ->
      (* let () =
        let s,x = REPR(Just 15) in
        let () = printf "%30s                              is       %s\n\n%!" s (generic_show x) in
        let s,x = REPR(q) in
        let () = printf "%30s                              is       %s\n\n%!" s (generic_show x) in
        let s,x = REPR(inj(lift 15)) in
        let () = printf "%30s                             is       %s\n\n%!" s (generic_show x) in
        let s,x = REPR(Just (inj(lift 15))) in
        let () = printf "%30s                             is       %s\n\n%!" s (generic_show x) in
        let s,x = REPR(Maybe.fmap (Just (inj(lift 15)))) in
        let () = printf "%30s                             is       %s\n\n%!" s (generic_show x) in
        let s,x = REPR(inj (Maybe.fmap (Just (inj(lift 15))))) in
        let () = printf "%30s                             is       %s\n%!" s (generic_show x) in
        ()
      in *)
      let () = printf "=========================\n%!" in
      let (right: (int maybe, int logic maybe logic,
                   int inner_logic maybe inner_logic) fancy)
        = inj @@ Maybe.fmap @@ (Just (inj@@lift 15)) in
      q === right)
    (fun qs -> printf "%s\n" (show_logic_maybe (show(inner_logic)@@show(int)) @@ Stream.hd qs))
;;
(*
(* let rec show_test t = show(test) (show(int)) t *)
@type ('a,'b) result = OK of 'a | Error of 'b with show
module Result = Fmap2(struct
  type ('a,'b) t = ('a,'b) result
  let fmap f g = function OK a -> OK (f a) | Error b -> Error (g b)
end);;

(* Lists ******************************* *)
@type ('a, 'b) alist = Nil | Cons of 'a * 'b with show

module F = Fmap2 (struct
  type ('a, 'b) t = ('a, 'b) alist
  let fmap f g = function
  | Nil -> Nil
  | Cons (x, y) -> Cons (f x, g y)
end)

let nil ()  = inj (F.fmap Nil)
let cons x y = inj (F.fmap (Cons (x, y)))

let rec show_intlist xs = show(alist) (show(int)) show_intlist xs
let (_: ((int, 'a) alist as 'a) -> bytes) = show_intlist

let test1 x =
  Fresh.one (fun y ->
    (x === cons (inj@@lift 5) y) &&&
    (y === nil ())
  ) *)
(*
let () =
    MiniKanren.run q test1
      (fun qs ->
        let (_:int) = qs in
        printf "%s\n" (show_intlist @@ Stream.hd qs))
  ;; *)
