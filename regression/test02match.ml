open Printf
open MiniKanren
open ImplicitPrinters
open Tester.M
(*
let (!) = embed

module Nat = struct
  type t = O | S of t logic
  let show = function
    | O -> "O"
    | S n -> sprintf "S (%s)" (show_logic_naive n)
end
implicit module Show_nat : (SHOW with type t = Nat.t) = Nat

let nat_of_int n : Nat.t =
  if n<0 then failwith "bad argument"
  else
    let rec helper acc n =
      if n=0 then acc
      else helper (Nat.S !acc) (n-1)
    in
    helper Nat.O n

let is_positive_nat n = fresh (_zero) (n === !(Nat.S _zero))
let is_nonnegative_nat n = fresh (_zero) (conde [(n === !(Nat.S _zero));  (n === !Nat.O) ])

module Peano_int = struct
    type t = bool * Nat.t logic
    let show : t -> string = fun (p,n) ->
      if p then show n
      else "-" ^ (show n)
    let of_int n =
      if n>=0 then (true, !(nat_of_int n) )
      else (false, !(nat_of_int (-n)) )
end

let is_positive_peano p =
  let open Nat in
  fresh (_zero) (p === !(true, !(S _zero)) )

let is_negative_peano p =
  let open Nat in
  fresh (_zero) (p === !(false, !(S _zero)) )

let is_nonnegative_peano p =
  let open Nat in
  conde [ p === !(true, !O)
        ; p === !(false, !O)
        ; is_positive_peano p
        ]

module MiniLambda = struct
  type structured_constant = Peano_int.t
  and lambda =
    | Lconst of structured_constant logic
end

implicit module Show_MiniLambda : (SHOW with type t = MiniLambda.lambda) =
struct
  type t = MiniLambda.lambda
  let show =
    let open MiniLambda in
    let rec helper = function
      | Lconst l -> sprintf "Lconst %s" (show_logic_naive l)
    in
    helper
end

let eval_lambda (lam_ast: MiniLambda.lambda) =
  let open MiniLambda in
  let open Tester.M in
  let rec evalo l (ans: MiniLambda.lambda logic) st =
    printf "evalo '%s' '%s'\n%!" (show l) (show ans);
    (ans === l) st
  in
  let open Tester.M.ConvenienceCurried in
  let open ImplicitPrinters in
  let stream = run one @@ evalo !(Lconst !(Peano_int.of_int 1)) in
  printf "stream    %s %d\n%!" __FILE__ __LINE__;
  (* printf "stream = '%s'\n%!" (MiniKanren.generic_show stream); *)
  (* let xs = stream |> MiniKanren.Stream.take ~n:1 *)
  (*          |> List.map (fun (_logger, (_q,_constraints)) -> _q) *)
  (* in *)
  let xs = stream (fun var1 -> var1 1 |> List.map (fun (_logger, (_q,_constraints)) -> _q) )
  in
  (* let (_:int list) = xs in *)
  (* let (_q,stream) = Tester.M.run (call_fresh (fun q st ->  evalo !(Lconst !(make_const 1)) q st,q) ) in *)
  (* let _ = Stream.take ~n:1 stream in *)
  printf "answers: %d\n%!" (List.length xs);
  List.iter (fun x -> print_endline @@ show x) xs

let () =
  let open MiniLambda in
  let lam2 = Lconst !(Peano_int.of_int 1) in
  let () = eval_lambda lam2 in
  ()
  *)
