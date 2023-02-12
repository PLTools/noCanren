open Peano

let rec length = function
  | [] -> 0
  | _ :: tl -> 1 + length tl
;;

let rec append x y =
  match x with
  | [] -> y
  | hd :: tl -> hd :: append tl y
;;

let rec member e = function
  | [] -> false
  | hd :: tl -> if hd = e then true else member e tl
;;

let rec rev = function
  | [] -> []
  | hd :: tl -> append (rev tl) [ hd ]
;;

let rec lookup key = function
  | [] -> None
  | (key0, value) :: tl -> if key = key0 then Some value else lookup key tl
;;

let rec assoc key = function
  | [] -> failwith "Key is not found"
  | (key0, value) :: tl -> if key = key0 then value else assoc key tl
;;

let rec map f = function
  | [] -> []
  | hd :: tl -> f hd :: map f tl
;;

let rec filter f = function
  | [] -> []
  | hd :: tl ->
    let filtered_tl = filter f tl in
    if f hd then hd :: filtered_tl else filtered_tl
;;

let rec fold_left f acc = function
  | [] -> acc
  | hd :: tl -> fold_left f (f acc hd) tl
;;

let rec fold_right f list acc =
  match list with
  | [] -> acc
  | hd :: tl -> f hd (fold_right f tl acc)
;;

let rec any f = function
  | [] -> false
  | hd :: tl -> f hd || any f tl
;;

let rec all f = function
  | [] -> true
  | hd :: tl -> f hd && all f tl
;;
