
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
           (*
  let output_html ~filename answers g =
    let module P=HTMLView in
    let empty = P.raw "" in
    let tag ?(attrs="") name (xs: P.viewer list) = P.(tag ~attrs name (seq xs)) in
    let span ?(attrs="") txt = P.(tag ~attrs "span" (seq [string txt])) in
    let script ?(attrs="") ~url = tag "script" ~attrs:(sprintf "src='%s' %s" url attrs) [] in
    let style ~url = tag "link" ~attrs:(sprintf "rel='stylesheet' type='text/css' href='%s' media='screen'" url) [] in
    let style_inline  text = tag "style"  ~attrs:"type='text/css'"        [P.raw text] in
    let script_inline text = tag "script" ~attrs:"type='text/javascript'" [P.raw text] in
    let div ?(attrs="") xs = tag ~attrs "div" xs in
    let ul  ?(attrs="") (xs: P.viewer list) = tag ~attrs "ul"  xs in

    let head = tag "head"
        [ style ~url:"web/main.css"
        ; script ~url:"web/runOnLoad.src.js"
        ; script ~url:"web/CollapsibleLists.compressed.js"
        ; script ~url:"web/jquery-1.11.3.min.js"
        ; script_inline "
runOnLoad(function(){
  console.log('1');
  CollapsibleLists.apply();
});

function onGenerationSelected(level) {
  console.log(level);
}
                         "]

    in

    let answers_div =
      div (List.mapi (fun i s ->
                      let attrs = sprintf "onclick='onGenerationSelected(%d)' class='level%d'" i i in
                      div ~attrs [P.string s]) answers)
    in

    let label ~name for_ = P.(tag ~attrs:"for='subfolder'" "label" (P.string name)) in
    let make_plock ~gen idx name xs =
      let name = sprintf "%s: %s" (string_of_node idx) name in
      match xs with
      | [] -> P.(li ~attrs:(sprintf "class='level%d'" gen) (string name))
      | xs ->
         let for_ = "subfolderfor1" in
         HTMLView.(li (seq [ P.string name
                           ; ul ~attrs:(sprintf "class='level%d proofnode' level='answer%d'" gen gen)  (seq xs)
                           ]) )
    in
    let rec helper node : HTMLView.er list =
      try let xs = find g node in
          let xs = List.rev xs in
          List.map (fun (dest,name,gen) -> make_plock ~gen dest name (helper dest)) xs
      with Not_found ->
        [HTMLView.string (sprintf "<No such node '%s'>" (string_of_node node))]
    in
    let p = HTMLView.seq (helper 0) in
    let p = HTMLView.ul ~attrs:"class='collapsibleList' id='roottree'" p in
    let p = HTMLView.(html (seq [head; body (seq [answers_div; p])])) in
    let ch = open_out filename in
    output_string ch (HTMLView.toHTML p);
    fprintf ch "\n%!" ;
    close_out ch

            *)

  let output_html ~filename _ _ =
    (* Firebug.console##log (Js.string "output html"); *)
    ()
end

let rewriter = Smart_logger.smart_logger [| |]

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

let run printer n runner goal =
  let graph = Logger.create () in
  M.run graph (fun st ->
    let (repr, result), vars = runner goal st in
    let (_: state Stream.t) = result in
    Printf.printf "%s, %s answer%s {\n"
      repr
      (if n = (-1) then "all" else string_of_int n)
      (if n <>  1  then "s" else "");

    let answers =
      GraphLogger.dump_graph (Obj.magic graph) stdout;
      for i=1 to n do
        ignore (take ~n:i result);
        GraphLogger.next_level (Obj.magic graph);
      done;
      take' ~n result
    in

    let text_answers =
      answers |> List.map
        (fun (st: State.t) ->
           let b = Buffer.create 100 in
           List.iter
             (fun (s, x) -> Printf.bprintf b "%s=%s; " s (printer st (refine st x)))
             vars;
           let s = Buffer.contents b in
           Printf.printf "%s\n%!" s;
           s
        )
    in

    Printf.printf "}\n%!";

    (* Firebug.console##log (Js.string "something nasty there") *)
    (* let out_file_prefix = Str.global_replace (Str.regexp " ") "_" repr in *)
    (* Logger.output_html  ~filename:(out_file_prefix^".html") text_answers graph; *)
  )
