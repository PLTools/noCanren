open Printf

type config = { mutable do_html: bool   (* html output of deduction tree *)
              ; mutable do_plain: bool  (* plain text output of deduction tree *)
              }
let config  = { do_html=false
              ; do_plain=false
              }

let () =
  let specs = [ ("-html",    Arg.Unit (fun () -> config.do_html  <- true), "html output")
              ; ("-text",    Arg.Unit (fun () -> config.do_plain <- true), "plain text output")
              ] in
  Arg.parse specs (fun _ -> ())
            "usage there"

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
    G.find g.l_graph node |> List.rev


  let single name = printf "@,@[<v 1>%s@]\n%!" name
  let start  name = printf "@,@[<v 1>%s\n%!" name
  let stop () = printf "@]%!"

  let output_plain ~filename g =
    let ch = open_out filename in
    let open Format in
    let ppf = formatter_of_out_channel ch in
    let rec iter_graph name xs =
      (* printf "iter_graph with name=%s, xs.len = %d\n%!" name (List.length xs); *)
      match xs with
      | [] -> fprintf ppf "@,@[<v 1>%s@]" name;
      | xs ->
         fprintf ppf "@,@[<v 1>%s" name;
         List.iter (fun (dest, name,_) ->
                    try let nodes = find g dest in
                        iter_graph name nodes;
                        fprintf ppf "@]"
                    with Not_found -> fprintf ppf "@,@[<v 1>%s@]" (string_of_node dest)
                   ) xs
    in
    iter_graph "root" (find g 0);
    force_newline ();
    close_out ch

  let output_html ~filename answers g =
    let module P=HTMLView in
    let tag ?(attrs="") name (xs: P.viewer list) = P.(tag ~attrs name (seq xs)) in
    let script ?(attrs="") ~url = tag "script" ~attrs:(sprintf "src='%s' %s" url attrs) [] in
    let style ~url = tag "link" ~attrs:(sprintf "rel='stylesheet' type='text/css' href='%s' media='screen'" url) [] in
    let script_inline text = tag "script" ~attrs:"type='text/javascript'" [P.raw text] in
    let div ?(attrs="") xs = tag ~attrs "div" xs in

    let head = tag "head"
        [ style  ~url:"web/main.css"
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

    let make_plock ~gen idx name xs =
      let name = sprintf "%s: %s" (string_of_node idx) name in
      match xs with
      | [] -> P.(li ~attrs:(sprintf "class='level%d'" gen) (string name))
      | xs ->
         (* let for_ = "subfolderfor1" in *)
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
end

module M = MiniKanren.Make(GraphLogger)

open MiniKanren
open ImplicitPrinters
open M



(** Some functions that use Convenience module *)
(** ****************************************** *)
open M.Convenience

let () =
  (* some code to check that types are good. nothing is really executed there *)
  let (goal3: 'a logic -> 'b logic -> 'c logic -> state -> _) = fun _ _ _ _ -> Obj.magic () in
  let ans () = run (succ @@ succ @@ succ zero) (fun q r s st -> goal3 q r s st) in
  ()

let run1 ~n (title, goal) =
  let (qf,_tl) = run (succ zero) goal in

  printf "'%s', asking for max %d results {\n%!" title n;
  List.iter (fun (loginfo,(q,_)) ->
      printf "q=%s\n%!" (show_logic_naive q);
    ) (qf _tl n);
  printf "}\n%!"

let run2 ~n (title,goal) =
  let qf,(rf,_tl) = M.Convenience.run (succ @@ succ zero) (fun q r st ->
    (* let foo stream : 'a M.PolyPairs.xxx * 'b M.PolyPairs.xxx = PolyPairs.((succ one) id) stream q r in *)
    goal q r st
  ) in

  let constraints_string logic =
    match logic with
    | Var {reifier;_} ->
      let (_,xs) = reifier () in
      sprintf "%s =/= all from [%s]" (show_logic_naive logic) (String.concat "," @@ List.map show_logic_naive xs)
    | _ -> ""
  in

  let string_of_constraints cs =
    match cs with
    | [] -> None
    | xs -> Some (String.concat ", " @@ List.map show_logic_naive xs)
  in

  printf "'%s', asking for max %d results {\n%!" title n;
  List.iter2 (fun (loginfo,(q,cs1)) (loginfo, (r,cs2)) ->
      printf "q=%s; r=%s\n%!" (show_logic_naive q) (show_logic_naive r);
      let cs c name =
        match string_of_constraints cs1 with
        | Some s -> printf "  when %s =/= anything from [%s]\n%!" name s
        | None -> ()
      in
      cs cs1 "q";
      cs cs2 "r";
      M.Logger.output_plain loginfo ~filename:".plain";
      M.Logger.output_html  [] loginfo ~filename:".html" ;

      (* printf "  when %s and %s\n%!" (constraints_string q) (constraints_string r); *)
    ) (qf _tl n) (rf _tl n);
  printf "}\n%!"


(** Some functions that use scary Convenience2 module *)
(** ****************************************** *)
open M

let run2 ~n (title,goal) =
  let open M.Convenience2 in
  let qf,(rf,_) = run (succ @@ succ zero) (fun q r st ->
    let foo stream : 'a PolyPairs.reifier * ('b PolyPairs.reifier * state Stream.t) = PolyPairs.(p (succ @@ succ zero)) stream q r in
    (goal q r st, foo)
  ) in

  (* let constraints_string logic = *)
  (*   match logic with *)
  (*   | Var {reifier;_} -> *)
  (*     let (_,xs) = reifier () in *)
  (*     sprintf "%s =/= all from [%s]" (show_logic_naive logic) (String.concat "," @@ List.map show_logic_naive xs) *)
  (*   | _ -> "" *)
  (* in *)

  (* let string_of_constraints cs = *)
  (*   match cs with *)
  (*   | [] -> None *)
  (*   | xs -> Some (String.concat ", " @@ List.map show_logic_naive xs) *)
  (* in *)

  printf "'%s', asking for max %d results {\n%!" title n;
  List.iter2 (fun (q,cs1) (r,cs2) ->
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
    ) (qf n) (rf n);
  printf "}\n%!"


(* let run reifier n runner goal = *)
(*   let graph = Logger.create () in *)
(*   M.run graph (fun st -> *)
(*     let (repr, result), vars = runner goal st in *)
(*     let (_: state Stream.t) = result in *)
(*     Printf.printf "%s, %s answer%s {\n" *)
(*       repr *)
(*       (if n = (-1) then "all" else string_of_int n) *)
(*       (if n <>  1  then "s" else ""); *)

(*     let answers = *)
(*       (\* GraphLogger.dump_graph (Obj.magic graph) stdout; *\) *)
(*       for i=1 to n do *)
(*         ignore (take' ~n:i result); *)
(*         GraphLogger.next_level (Obj.magic graph); *)
(*       done; *)
(*       take' ~n result *)
(*     in *)

(*     let text_answers = *)
(*       answers |> List.map *)
(*         (fun (st: State.t) -> *)
(*            let s = List.map *)
(*             (fun (s, x) -> *)
(*               let v, dc = refine st x in *)
(*               (\* let pv = printer v in *\) *)
(*               match reifier dc v with *)
(*               | "" -> sprintf "%s=%s;" s (show_logic_naive v) *)
(*               | r  -> sprintf "%s=%s (%s);" s (show_logic_naive v) r *)
(*              ) *)
(*              vars |> String.concat " " *)
(*            in *)
(*            Printf.printf "%s\n%!" s; *)
(*            s *)
(*         ) *)
(*     in *)


(*     Printf.printf "}\n%!"; *)

(*     (\* GraphLogger.dump_graph (Obj.magic graph) stdout; *\) *)
(*     if config.do_plain then *)
(*       let out_file_prefix = Str.global_replace (Str.regexp " ") "_" repr in *)
(*       let _ = Logger.output_plain ~filename:(out_file_prefix^".plain") graph in *)
(*       (); *)
(*     if config.do_html *)
(*     then Logger.output_html  ~filename:(out_file_prefix^".html") text_answers graph; *)
(*   ) *)
