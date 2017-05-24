open MiniKanren
open Tester
open Printf
open ManualReifiers

let (===) = unitrace (fun h x ->
  GT.show logic string_of_int @@ ManualReifiers.int_reifier h x)


let () =
  let c = Gc.get () in
  Gc.set { c with Gc.verbose = 0x400 }



































































(*
let rec evalo m =
  fresh (f2)
    (conde
        [ fresh (x)
            (f2 === !!1)
        (* ; fresh (p)
            (f2 === !!2) *)
        ; fresh (p2)
            (f2 === !!3)
        ])
    (m === !!5 )
;;
*)

let rec evalo m st =
  let (f2,idx) = State.new_var st  in
  printfn "create new variable f2 as _.%d" idx;
  printfn "create inc   in fresh === (f2)";
  MKStream.inc (fun () ->
      printfn "inc in fresh forced === (f2)";
      printfn " created inc in conde";
      MKStream.bind
        (MKStream.inc (fun ()  ->
              printfn " force a conde";
              MKStream.mplus
                 (let (x,idx) = State.new_var st  in
                  printfn "create new variable x as _.%d" idx;
                  printfn "create inc   in fresh === (x)";
                  MKStream.inc (fun ()  ->
                      printfn "inc in fresh forced === (x)";
                      (f2 === (!! 1)) st))
                 (MKStream.inc (fun ()  ->
                       printfn " force inc from mplus* I";
                       (let (p2,idx) = State.new_var st  in
                        printfn "create new variable p2 as _.%d" idx;
                        printfn "create inc   in fresh === (p2)";
                        MKStream.inc (fun ()  ->
                            printfn "inc in fresh forced === (p2)";
                            (f2 === (!! 3)) st))))))
        (m === (!! 5))
  )


let () =
  run q evalo (fun qs -> Stream.take ~n:2 qs
    |> List.map (fun rr -> rr#prj) |> List.iter (printfn "%d") )





let () = ()
