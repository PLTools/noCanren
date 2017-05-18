open MiniKanren
open Tester
open Printf
open ManualReifiers
open GT

let (===) = unitrace (fun h x -> GT.show logic string_of_int @@ ManualReifiers.int_reifier h x)

(* let rec evalo m =
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

(* let rec evalo m =
  printfn " applying evalo to m";
  (fun st  ->
    let (f2,idx) = State.new_var st  in
    printfn "create new variable %s as _.%d" "f2" idx;
    (let () = printfn "create inc   in fresh === (%s)" "f2"  in
      MKStream.inc
        (fun ()  ->
           let () = printfn "inc in fresh forced === (%s)" "f2"  in
           bind_star2
              ((fun st ->
                (* printfn "created inc conde"; *)
                MKStream.inc (fun () ->
                 printfn " force a conde";
                 my_mplus_star
                  [ fun st ->
                     let (x,idx) = State.new_var st  in
                     printfn "create new variable %s as _.%d" "x" idx;
                     (let () = printfn "create inc   in fresh === (%s)" "x" in
                      MKStream.inc
                        (fun ()  ->
                           let () = printfn "inc in fresh forced === (%s)" "x"  in
                           (f2 === (!! 1)) st))
                  ; fun st  ->
                    let (p,idx) = State.new_var st  in
                    printfn "create new variable %s as _.%d" "p" idx;
                    (let () = printfn "create inc   in fresh === (%s)" "p" in
                     MKStream.inc
                       (fun ()  ->
                          let () = printfn "inc in fresh forced === (%s)" "p"
                             in
                          (f2 === (!! 2)) st))
                  ] st) ))
              [evalo (!! 4)]))) *)


let rec evalo m =
  printfn " applying evalo to m";
  (fun st  ->
    let (f2,idx) = State.new_var st  in
    printfn "create new variable %s as _.%d" "f2" idx;
    (let () = printfn "create inc   in fresh === (%s)" "f2"  in
      MKStream.inc
        (fun ()  ->
           let () = printfn "inc in fresh forced === (%s)" "f2"  in
           MiniKanren.bind_star2
              (fun st ->
                (* printfn "created inc conde"; *)
                (* MKStream.inc (fun () -> my_mplus_star [] st) *)
                MKStream.inc (fun () -> ())
              )
              [evalo (!! 4)])))


let () =
  run q evalo (fun qs -> Stream.take ~n:1 qs
    |> List.map (fun rr -> rr#prj) |> List.iter (printfn "%d") )





let () = ()
