type 'a logic = Var of int
              | Value of 'a
[@@deriving showT]
;;
let pppt_logic : string * (Format.formatter -> 'a -> unit) ->
  string *  (Format.formatter -> 'a logic -> unit) = fun (argname, pp_arg) ->
  ( Printf.sprintf "%s logic" argname
  , fun fmt -> function
    | Var n -> Format.fprintf fmt "(_.%d : %s)" n argname
    | Value x ->
        Format.fprintf fmt "Value (";
        pp_arg fmt x;
        Format.fprintf fmt ")"
  )
(*
let pppt_int : string * (Format.formatter -> int -> unit) =
  ("int", (fun fmt -> Format.fprintf fmt "%d"))

let pppt_string : string * (Format.formatter -> string -> unit) =
  ("string", (fun fmt -> Format.fprintf fmt "%s" ))

type ('a, 'b) glist = Nil | Cons of 'a * 'b
(* [@@deriving showT] *)

let pppt_glist:  string * (Format.formatter -> 'a -> unit) ->
    string * (Format.formatter -> 'b -> unit) ->
    string * (Format.formatter -> ('a,'b) glist -> unit) = fun (typ1, pp1) (typ2, pp2) ->
  ( Printf.sprintf "(%s,%s) llist" typ1 typ2
  , fun fmt -> function
    | Nil -> Format.fprintf fmt "[]"
    | Cons (a,b) -> Format.fprintf fmt "%a :: %a" pp1 a pp2 b
  )

type 'a list = ('a, 'a list) glist

let rec pppt_list : string * (Format.formatter -> 'a -> unit) ->
  string * (Format.formatter -> 'a list -> unit) = fun (typ1, pp1) ->
  pppt_glist (typ1,pp1)
    ( Printf.sprintf " %s list" typ1
    , fun fmt xs -> (snd @@ pppt_list (typ1,pp1)) fmt xs )

let rec pppt_intlogic_list : string * (Format.formatter -> int logic list -> unit) =
  ("int logic list"
  , fun fmt xs -> (pppt_list (pppt_logic pppt_int) |> snd) fmt xs
  )

type 'a llist = ('a, 'a llist) glist logic
let rec pppt_intlogic_llist : string * (Format.formatter -> int logic llist -> unit) =
  ("int logic llist"
  , fun fmt -> (snd @@ pppt_logic (pppt_glist (pppt_logic pppt_int) pppt_intlogic_llist)) fmt
  )


let () =
  Format.fprintf Format.std_formatter "%a\n%!" (pppt_logic pppt_int    |> snd ) (Value 1);
  Format.fprintf Format.std_formatter "%a\n%!" (pppt_logic pppt_int    |> snd ) (Var   2);
  Format.fprintf Format.std_formatter "%a\n%!" (pppt_logic pppt_string |> snd ) (Var  33);
  Format.fprintf Format.std_formatter "%a\n%!" (pppt_list  pppt_string |> snd )
    (Cons ("a", Cons ("b", Nil)));
  Format.fprintf Format.std_formatter "%a\n%!" (pppt_intlogic_list |> snd )
    (Cons (Value 10, Cons (Var 1, Nil)));
  Format.fprintf Format.std_formatter "%a\n%!" (pppt_intlogic_llist |> snd )
    (Value (Cons (Value 10, Value (Cons (Value 20, Var 21)))));
*)
