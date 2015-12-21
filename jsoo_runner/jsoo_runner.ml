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
      let level_class  = Printf.sprintf "level%d" gen in

      match xs with
      | [] ->
        let open T in
        li ~a:[a_class [level_class]] [pcdata name]
      | xs ->
         let for_ = "subfolderfor1" in
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

let dumb_ppx = Ast_mapper.({ default_mapper with structure_item=fun _ i -> Printf.printf "A\n%!"; i});;

open MiniKanren
module M = MiniKanren.Make(GraphLogger)
open M

let succ prev f = call_fresh (fun x -> prev (f x))

let zero  f = f
let one   f = succ zero f
let two   f = succ one f
let three f = succ two f
let four  f = succ three f

let q    = one
let qp   = two
let qpr  = three
let qprt = four

let run printer reifier n runner goal =
  let graph = Logger.create () in
  M.run graph (fun st ->
    let (result, vars) = runner goal st in
    let (_: state Stream.t) = result in
    Printf.printf "%s answer%s {\n"
      (if n = (-1) then "all" else string_of_int n)
      (if n <>  1  then "s" else "");

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
           let b = Buffer.create 100 in
           let s = List.map
            (fun (s, x) ->
              let v, dc = refine st x in
              let pv = printer v in
              match reifier dc v with
              | "" -> sprintf "%s=%s;" s pv
              | r  -> sprintf "%s=%s (%s);" s pv r
             )
             vars |> String.concat " "
           in
           Printf.printf "%s\n%!" s;
           s
        )
    in

    Printf.printf "}\n%!";
    M.Logger.output_html ~filename:"" text_answers graph
  )

let empty_reifier _ _ = ""

let show_int      = GT.( show(logic) (show int) )
let show_int_list = GT.( show(logic) (show(llist) (show int)) )

let _ =
  let a_and_b a : state -> state MiniKanren.Stream.t =
    call_fresh
      (fun b -> conj (a === !7)
                     (disj (b === !6)
                           (b === !5)
                     )
      )
  in

  fun () -> run show_int empty_reifier 1 q (fun q st -> a_and_b q st, ["q", q])
