open Printf

module GraphLogger = struct
  module Int = struct
    type t = int
    let compare: t->t->int  = compare
    let hash   : t->int  = fun i -> i land max_int
    let equal  : t->t->bool = (=)
  end
  module G = Hashtbl.Make(Int)
  type node = int
  type node_level = int
  type node_info = string

  type t =
    { l_graph: (node*node_info*node_level) list G.t
    ; mutable l_counter: int
    ; mutable l_level: int;
    }

  let next_level g = g.l_level <- 1+g.l_level

  let string_of_node n = string_of_int n

  let create () = { l_graph=G.create 97; l_counter=0; l_level=0 }

  let make_node g =
    let rez = g.l_counter in
    G.add g.l_graph g.l_counter [] ;
    g.l_counter <- rez+1;
    rez

  let connect: t -> node -> node -> string -> unit = fun g from dest msg ->
    try let ans = G.find g.l_graph from in
        G.replace g.l_graph from ((dest,msg,g.l_level) :: ans)
    with Not_found -> G.add g.l_graph from [(dest,msg,g.l_level)]


  open Printf

  let dump_graph g ch =
    print_endline "dumping graph";
    let f k v =
      fprintf ch "%d: [" k;
      List.iteri (fun n (dest,name,_level) ->
                  if n<>0 then fprintf ch ",";
                  fprintf ch "(%d,'%s',%d)" dest name _level) v;
      fprintf ch " ]\n"
    in
    G.iter f g.l_graph;
    flush ch

  let find g node =
    G.find g.l_graph node

  let output_plain ~filename _ = ()
  let output_html ~filename _answers g =
    let module T = Tyxml_js.Html5 in
    let open Tyxml_js.Html5 in
    let make_plock ~gen idx name xs =
      let name = sprintf "%s: %s" (string_of_node idx) name in
      (* let level_class  = Printf.sprintf "level%d" gen in *)

      match xs with
      | [] ->
        let open T in
        T.(li (* ~a:[a_class [level_class]] *) [pcdata name])
      | xs ->
         T.(li [ pcdata name
               ; ul ~a:[] xs
               ])
    in
    let rec helper node =
      try let xs = find g node in
          let xs = List.rev xs in
          List.map (fun (dest,name,gen) -> make_plock ~gen dest name (helper dest)) xs
      with Not_found ->
        T.[li [pcdata (sprintf "<No such node '%s'>" (string_of_node node))] ]
    in
    let root = Dom_html.getElementById "listContainer" in
    let () = root##.innerHTML := Js.string "" in

    let deduction_tree =
      T.(ul ~a:[a_id "expList"] (helper 0))
    in
    let (_: Dom.node Js.t) =
      root##appendChild ((Tyxml_js.To_dom.of_ul deduction_tree) :> Dom.node Js.t)
    in
    let _ = Js.Unsafe.eval_string "prepareList();" in
    ()

end

let smart_ppx = Smart_logger.smart_logger [| |]
let pa_minikanren_ppx = Smart_logger.pa_minikanren [| |]
let repr_ppx = Ppx_repr.mapper

let dumb_ppx = Ast_mapper.({ default_mapper with structure_item=fun _ i -> Printf.printf "A\n%!"; i});;

open MiniKanrenImpl
module M = Make(GraphLogger)
open M
open M.Convenience4

let run1 ~n (title, goal) =
  let open M.Convenience4 in
  printf "'%s', asking for max %d results {\n%!" title n;
  run one goal
    |> (fun stream -> Stream.take ~n stream |> List.iter
          (fun (q,_constr) ->
            printf "q=%s\n%!" (show_logic_naive q);
          )
       );
  printf "}\n%!"

let run2 ~n (title,goal) =
  let open M.Convenience4 in
  printf "'%s', asking for max %d results {\n%!" title n;
  run (succ one) goal |>
    begin fun stream ->
     Stream.take ~n stream |> List.iter
        (fun ((q,cs1),(r,cs2)) ->
           printf "q=%s; r=%s\n%!" (show_logic_naive q) (show_logic_naive r);
           (* let cs c name = *)
           (*   match string_of_constraints cs1 with *)
           (*   | Some s -> printf "  when %s =/= anything from [%s]\n%!" name s *)
           (*   | None -> () *)
           (* in *)
           (* cs cs1 "q"; *)
           (* cs cs2 "r"; *)
           (* M.Logger.output_plain loginfo ~filename:".plain"; *)
           (* M.Logger.output_html  [] loginfo ~filename:".html" ; *)

           (* printf "  when %s and %s\n%!" (constraints_string q) (constraints_string r); *)

        )
    end;
  printf "}\n%!"
(*
let run ?(varnames=[]) n num (repr,goal) =
  let open M.Convenience4 in
  run (fun st ->
    let result = runner goal st in

    Printf.printf "%s answer%s {\n"
      (if n = (-1) then "all" else string_of_int n)
      (if n <>  1  then "s" else "");
(*
    let vars' = make_var_pairs ~varnames stor in
    let answers =
      (* GraphLogger.dump_graph (Obj.magic graph) stdout; *)
      for i=1 to n do
        ignore (take' ~n:i result);
        GraphLogger.next_level (Obj.magic graph);
      done;
      take' ~n result
    in

    let text_answers =
      answers |> List.map
        (fun (st: State.t) ->
           let s = List.map
            (fun (x, varname) ->
              let rez, _dc = refine st x in
              sprintf "%s=%s;" varname (show_logic_naive rez)
             )
             vars' |> String.concat " "
           in
           printf "%s\n%!" s;
           s
        )
    in *)

    Printf.printf "}\n%!";
    M.Logger.output_html ~filename:"" (* text_answers *) [] graph;
    result
  )

open ImplicitPrinters
(*
let _ =
  let a_and_b a : M.goal =
    call_fresh
      (fun b -> conj (a === embed 7)
                     (disj (b === embed 6)
                           (b === embed 5)
                     )
      )
  in

  fun () -> run 1 one ("descr",a_and_b)
*)
*)
