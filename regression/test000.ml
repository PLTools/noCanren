external (|!): 'a -> ('a -> 'b) -> 'b = "%revapply"

(*
module OstapLogger: MiniKanren.LOGGER = struct
  open Ostap.Pretty
  type t = Ostap.Pretty.printer
  let empty = Ostap.Pretty.empty
  let log name xs =
    [string name; vboxed (listByBreak xs)] |! seq |! hboxed
  let to_string = toString
end
 *)

module GraphLogger(* : MiniKanren.LOGGER *) = struct
  module Int = struct
    type t = int
    let compare: t->t->int  = compare
    let hash   : t->int  = fun i -> i land max_int
    let equal  : t->t->bool = (=)
  end
  module G = Hashtbl.Make(Int)
  let graph: (int*string) list G.t = G.create 17
  let clear () = G.clear graph

  type t = int

  let string_of_node : t->string = string_of_int

  let node_counter = ref 0
  let create () =
    let rez = !node_counter in
    G.add graph rez [] ;
    incr node_counter;
    rez

  let connect from dest msg =
    try let ans = G.find graph from in
        G.replace graph from ((dest,msg) :: ans)
    with Not_found -> G.add graph from [(dest,msg)]


  open Printf

  let dump_graph ch =
    let f k v =
      fprintf ch "%d: [" k;
      List.iteri (fun n (dest,name) ->
                  if n<>0 then fprintf ch ",";
                  fprintf ch "(%d,'%s')" dest name) v;
      fprintf ch " ]\n"
    in
    G.iter f graph;
    flush ch

  let find node =
    printf "called find of node '%s'\n%!" (string_of_node node);
    G.find graph node

  open Ostap.Pretty
  let to_channel filename =
    let make_plock idx name xs =
      let name = sprintf "%s: %s" (string_of_node idx) name in
      match xs with
      | [] -> string name
      | xs -> plock (string name) (boxed (listByBreak xs))
    in
    let rec helper node : printer list =
      try let xs = find node in
          let xs = List.rev xs in
          List.map (fun (dest,name) -> make_plock dest name (helper dest)) xs
      with Not_found -> (* failwith "bad graph" *)
        [string (sprintf "<No such node '%s'>" (string_of_node node))]
    in
    let p = make_plock 0 "root" (helper 0) in
    let ch = open_out filename in
    output_string ch (toString p);
    fprintf ch "\n%!" ;
    close_out ch
end


(* open GT *)
module M = MiniKanren.Make(GraphLogger)
open MiniKanren
open M
open Printf

let run2 memo printer n (goal: _ -> _ -> M.goal) =
  run (
    call_fresh_named "q" (fun q ->
      call_fresh_named "r" (fun r st ->
        let result = take' ~n:n (goal q r st) in
        Printf.printf "%s {\n" memo;
        List.iteri
          (fun i st ->
             let (_: M.State.t) = st in
             GraphLogger.dump_graph stdout;
             GraphLogger.to_channel (sprintf "out%d" i);

             Printf.printf "q=%s, r=%s\n" (printer st(refine st q)) (printer st(refine st r))
          )
          result;
          GraphLogger.clear ();
        Printf.printf "}\n%!"
  )))

let run1 memo printer n (goal: _ -> M.goal) =
  run (
    call_fresh_named "q" (fun q st ->
      let result = take' ~n:n (goal q st) in
      Printf.printf "%s {\n" memo;
      List.iteri (fun i st ->
        GraphLogger.dump_graph stdout;
        GraphLogger.to_channel (sprintf "out%d" i);

        Printf.printf "q=%s\n" (printer st (refine st q))
      )
      result;
      GraphLogger.clear ();
      Printf.printf "}\n%!"
  ))

let just_a a = a === 5

let a_and_b a =
  call_fresh_named "b" (fun b ->
      conj (a === 7)
           (disj (b === 6)
                 (b === 5)
           )
  )

let a_and_b' b =
  call_fresh_named "a" (fun a ->
      conj (a === 7)
           (disj (b === 6)
                 (b === 5)
           )
  )

let rec fives x =
  disj (x === 5)
       (fun st -> Stream.from_fun (fun () -> fives x st))

let rec appendo a b ab =
  "appendo" <=>
  disj
    (conj (a === []) (b === ab) )
    (call_fresh (fun h ->
      (call_fresh (fun t ->
        (conj (a === h::t)
           (call_fresh (fun ab' ->
              conj (h::ab' === ab)
                   (fun st -> Stream.from_fun (fun () -> appendo t b ab' st))
           ))
      )))
    ))

let rec reverso a b =
  "reverso" <=>
  disj
    (conj (a === []) (b === []))
    (call_fresh_named "h" (fun h ->
      (call_fresh_named "t" (fun t ->
          (conj (a === h::t)
              (call_fresh_named "a'" (fun a' ->
                 conj (fun st -> Stream.from_fun (fun () -> appendo a' [h] b st))
                      (fun st -> Stream.from_fun (fun () -> reverso t a' st))
              ))
        )
    )
    )))

let int_list st l = mkshow(list) (mkshow(int)) st l

let _ =
  (* run1 "reverso q q max 1 result"            int_list       1 (fun q   -> reverso q q); *)
  (* run1 "just_a"                              (mkshow(int))  1 (fun q   -> just_a q); *)
  run1 "a_and_b'"                            (mkshow(int))  2 (fun q   -> a_and_b' q);


   (* run1 "appendo q [3; 4] [1; 2; 3; 4] max 1 result" int_list       1 (fun q   -> appendo q [3; 4] [1; 2; 3; 4]); *)
   (* run2 "appendo q [] r max 4 results"               int_list       4 (fun q r -> appendo q [] r); *)
   (* run1 "reverso q [1; 2; 3; 4] max 1 result"        int_list       1 (fun q   -> reverso q [1; 2; 3; 4]); *)
   (* run1 "reverso [] [] max 1 result"                 int_list       1 (fun q   -> reverso [] []); *)
   (* run1 "reverso [1; 2; 3; 4] q max 1 result"        int_list       1 (fun q   -> reverso [1; 2; 3; 4] q); *)
   (* run1 "reverso q q max 2 result"                   int_list       2 (fun q   -> reverso q q); *)
   (* run1 "reverso q q max 3 result"                   int_list       3 (fun q   -> reverso q q); *)
   (* run1 "reverso q q max 10 results"                 int_list      10 (fun q   -> reverso q q); *)
   (* run1 "reverso q [1] max 2 results"                int_list       2 (fun q   -> reverso q [1]); *)
   (* run1 "reverso [1] q max 2 results"                int_list       1 (fun q   -> reverso [1] q); *)
   (* run1 "just_a"                                     (mkshow(int))  1 (fun q   -> just_a q); *)
   (* run1 "a_and_b"                                    (mkshow(int))  1 (fun q   -> a_and_b q); *)
   (* run1 "fives"                                      (mkshow(int)) 10 (fun q   -> fives q); *)
   ()
