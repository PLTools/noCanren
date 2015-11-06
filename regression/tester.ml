module GraphLogger = struct
  module Int = struct
    type t = int
    let compare: t->t->int  = compare
    let hash   : t->int  = fun i -> i land max_int
    let equal  : t->t->bool = (=)
  end
  module G = Hashtbl.Make(Int)
  type t =
    { l_graph: (int*string) list G.t
    ; mutable l_counter: int
    }

  type node = int

  let string_of_node n = string_of_int n

  let create () = { l_graph=G.create 17; l_counter=0 }

  let make_node g =
    let rez = g.l_counter in
    G.add g.l_graph g.l_counter [] ;
    g.l_counter <- rez+1;
    rez

  let connect: t -> node -> node -> string -> unit = fun g from dest msg ->
    try let ans = G.find g.l_graph from in
        G.replace g.l_graph from ((dest,msg) :: ans)
    with Not_found -> G.add g.l_graph from [(dest,msg)]


  open Printf

  let dump_graph g ch =
    let f k v =
      fprintf ch "%d: [" k;
      List.iteri (fun n (dest,name) ->
                  if n<>0 then fprintf ch ",";
                  fprintf ch "(%d,'%s')" dest name) v;
      fprintf ch " ]\n"
    in
    G.iter f g.l_graph;
    flush ch

  let find g node =
    G.find g.l_graph node

  open Ostap.Pretty
  let output_plain ~filename g =
    let make_plock idx name xs =
      let name = sprintf "%s: %s" (string_of_node idx) name in
      match xs with
      | [] -> string name
      | xs -> plock (string name) (boxed (listByBreak xs))
    in
    let rec helper node : printer list =
      try let xs = find g node in
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

  let output_html ~filename g =
    let label ~name for_ = HTMLView.(tag ~attrs:"for='subfolder'" "label" (string name)) in
    let make_plock idx name xs =
      let name = sprintf "%s: %s" (string_of_node idx) name in
      match xs with
      | [] -> HTMLView.(li ~attrs:"class='file'" (anchor "" (string name)))
      | xs ->
         let for_ = "subfolderfor1" in
         HTMLView.(li (seq [ label ~name for_
                           ; input ~attrs:(sprintf "type='checkbox' id='%s'" for_) (raw"")
                           ; ol (seq xs)
                           ]) )
    in
    let rec helper node : HTMLView.er list =
      try let xs = find g node in
          let xs = List.rev xs in
          List.map (fun (dest,name) -> make_plock dest name (helper dest)) xs
      with Not_found ->
        [HTMLView.string (sprintf "<No such node '%s'>" (string_of_node node))]
    in
    let p = (* make_plock 0 "root" *) HTMLView.seq (helper 0) in
    let p = HTMLView.ol ~attrs:"class='tree'" p in
    let head =
      HTMLView.(tag "head" (
        tag ~attrs:"rel='stylesheet' type='text/css' href='web/_styles.css' media='screen'" "link" HTMLView.br))
    in
    let p = HTMLView.(html (seq [head;body p])) in
    let ch = open_out filename in
    output_string ch (HTMLView.toHTML p);
    fprintf ch "\n%!" ;
    close_out ch
end


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
  run graph (fun st ->
    let (repr, result), vars = runner goal st in
    let (_: state Stream.t) = result in
    Printf.printf "%s, %s answer%s {\n"
      repr
      (if n = (-1) then "all" else string_of_int n)
      (if n <>  1  then "s" else "");
    List.iter
      (fun st ->
         List.iter
           (fun (s, x) -> Printf.printf "%s=%s; " s (printer st (refine st x)))
           vars;
         Printf.printf "\n"
      )
      (take' ~n:n result);
    Printf.printf "}\n%!"
  )
