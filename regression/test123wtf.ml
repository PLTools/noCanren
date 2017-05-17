open MiniKanren
open Tester
open Printf
open ManualReifiers
open GT

(* let (===) = unitrace (fun h x -> GT.show logic string_of_int @@ ManualReifiers.int_reifier h x) *)

let rec evalo m =
  printfn " applying evalo to m";
  fresh (f2)
    (conde
        [ fresh (x)
            (f2 === !!1)
        ; fresh (p)
            (f2 === !!2)
        ])
    (evalo !!4 )
;;
(*
let rec evalo m =
  printfn " applying evalo to m";
  (fun st  ->
     let f2 = State.new_var st  in
     MKStream.inc
       (fun ()  ->
          bind_star2
          ((conde
              [(fun st  ->
                  let x = State.new_var st  in
                  MKStream.inc (fun ()  -> (f2 === (!! 1)) st));
              (fun st  ->
                 let p = State.new_var st  in
                 MKStream.inc (fun ()  -> (f2 === (!! 2)) st))]) st)
          (evalo (!! 4))))
*)

let () =
  run q evalo (fun qs -> Stream.take ~n:1 qs
    |> List.map (fun rr -> rr#prj) |> List.iter (printfn "%d") )
