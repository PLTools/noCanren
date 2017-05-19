open MiniKanren
open Tester
open Printf
open ManualReifiers
open GT

let (===) = unitrace (fun h x -> GT.show logic string_of_int @@ ManualReifiers.int_reifier h x)






































































(*
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
;; *)

let rec evalo m =
  (* printfn " applying evalo to m"; *)
  (fun st  ->
    let (f2,idx) = State.new_var st  in
    printfn "create new variable f2 as _.%d" idx;
    printfn "create inc   in fresh === (f2)";
    MKStream.inc (fun ()  ->
          printfn "inc in fresh forced === (f2)";
          bind_star2
            ( printfn " created inc in conde";
              MKStream.inc3 (fun st () ->
                printfn " force a conde";
                MKStream.mplus
                  ( let (x,idx) = State.new_var st  in
                    let () = printfn "create new variable x as _.%d" idx in
                    let () = printfn "create inc   in fresh === (x)" in
                    MKStream.inc3 (fun st () ->
                      printfn "inc in fresh forced === (x)";
                      (f2 === (!! 1)) st
                    ) st
                  )
                  (MKStream.inc (fun () ->
                    printfn "force inc from mplus*";
                    let (p,idx) = State.new_var st  in
                    printfn "create new variable p as _.%d" idx;
                    printfn "create inc   in fresh === (p)";
                    MKStream.inc3 (fun st ()  ->
                      printfn "inc in fresh forced === (p)";
                      (f2 === (!! 2)) st
                    ) st
                  ))) st)
            [
              (m === (!! 1))
              (* evalo2 (!! 4) *)
            ]
      ))

let () =
  run q evalo (fun qs -> Stream.take ~n:1 qs
    |> List.map (fun rr -> rr#prj) |> List.iter (printfn "%d") )





let () = ()
