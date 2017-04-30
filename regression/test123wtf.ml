open MiniKanren
open Tester
open Printf
open ManualReifiers

let (===) ?loc = unitrace (fun h x ->
  GT.show logic string_of_int @@ ManualReifiers.int_reifier h x)


let () =
  let c = Gc.get () in
  Gc.set { c with Gc.verbose = 0x400 }




































































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




let () =
  run q evalo (fun qs -> Stream.take ~n:2 qs
    |> List.map (fun rr -> rr#prj) |> List.iter (printfn "%d") )





let () = ()
