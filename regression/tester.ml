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

  let create () = { l_graph=G.create 17; l_counter=0; l_level=0 }

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
    let f k v =
      fprintf ch "%d: [" k;
      List.iteri (fun n (dest,name,_level) ->
                  if n<>0 then fprintf ch ",";
                  fprintf ch "(%d,'%s')" dest name) v;
      fprintf ch " ]\n"
    in
    G.iter f g.l_graph;
    flush ch

  let find g node =
    G.find g.l_graph node

  module P = Ostap.Pretty

  let output_plain ~filename g =
    let make_plock idx name xs =
      let name = sprintf "%s: %s" (string_of_node idx) name in
      match xs with
      | [] -> P.string name
      | xs -> P.plock (P.string name) (P.boxed (P.listByBreak xs))
    in
    let rec helper node : P.printer list =
      try let xs = find g node in
          let xs = List.rev xs in
          List.map (fun (dest,name,_) -> make_plock dest name (helper dest)) xs
      with Not_found -> (* failwith "bad graph" *)
        [P.string (sprintf "<No such node '%s'>" (string_of_node node))]
    in
    let p = make_plock 0 "root" (helper 0) in
    let ch = open_out filename in
    output_string ch (P.toString p);
    fprintf ch "\n%!" ;
    close_out ch


  let output_html ~filename answers g =
    let module P=HTMLView in
    let empty = P.raw "" in
    let tag ?(attrs="") name xs = P.(tag ~attrs name (seq xs)) in
    let span ?(attrs="") txt = P.(tag ~attrs "span" (seq [string txt])) in
    let div xs = tag "div" xs in

    let head = tag "head"
        [ tag "link" ~attrs:"rel='stylesheet' type='text/css' href='web/mktree.css' media='screen'" [empty]
        (* ; tag "script" ~attrs:"src='//code.jquery.com/jquery-1.11.3.min.js'" [HTMLView.br] *)
        ; tag "script" ~attrs:"src='web/mktree.js'" [empty]
        ; tag "script" ~attrs:"type='text/javascript'" [P.raw "
//$('#proof_tree')
//addEvent(window,'load',convertTrees);
addEvent(window,'load', function() { convertTree(document.getElementById('roottree'), false); });
"]
        ]
    in

    let answers_div =
      div (List.map (fun s -> div [P.string s]) answers)
    in

    let label ~name for_ = P.(tag ~attrs:"for='subfolder'" "label" (P.string name)) in
    let make_plock idx name xs =
      let name = sprintf "%s: %s" (string_of_node idx) name in
      match xs with
      | [] -> HTMLView.(li ~attrs:"class='file'" (anchor "" (string name)))
      | xs ->
         let for_ = "subfolderfor1" in
         HTMLView.(li (seq [ (* span ~attrs:"class='bullet'" "&nbsp;" *)
                           (* ; input ~attrs:(sprintf "type='checkbox' id='%s'" for_) (raw"") *)
                           P.b (P.string name)
                           ; ul (seq xs)
                           ]) )
    in
    let rec helper node : HTMLView.er list =
      try let xs = find g node in
          let xs = List.rev xs in
          List.map (fun (dest,name,_) -> make_plock dest name (helper dest)) xs
      with Not_found ->
        [HTMLView.string (sprintf "<No such node '%s'>" (string_of_node node))]
    in
    let p = HTMLView.seq (helper 0) in
    (* let p = HTMLView.ul ~attrs:"class='proof_tree mktree'" p in *)
    let p = HTMLView.ul ~attrs:"class='mktree' id='roottree'" p in
    let p = HTMLView.(html (seq [head; body (seq [answers_div; p])])) in
    let ch = open_out filename in
    output_string ch (HTMLView.toHTML p);
    fprintf ch "\n%!" ;
    close_out ch
end

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
           List.iter
             (fun (s, x) -> Printf.bprintf b "%s=%s; " s (printer st (refine st x)))
             vars;
           let s = Buffer.contents b in
           Printf.printf "%s\n%!" s;
           s
        )
    in


    Printf.printf "}\n%!";
    let out_file_prefix = Str.global_replace (Str.regexp " ") "_" repr in
    Logger.output_plain ~filename:(out_file_prefix^".plain") graph;
    Logger.output_html  ~filename:(out_file_prefix^".html") text_answers graph;
  )
