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

let rec mem e = function
  | [] -> false
  | hd :: tl -> if hd = e then true else mem e tl
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

let mapi f =
  let rec mapi f index = function
    | [] -> []
    | x :: xs -> f index x :: mapi f (index + 1) xs
  in
  mapi f 0
;;

let rec map2 f l1 l2 =
  match l1, l2 with
  | [], [] -> []
  | hd1 :: tl1, hd2 :: tl2 -> f hd1 hd2 :: map2 f tl1 tl2
  | _ :: _, [] | [], _ :: _ -> failwith "Invalid_argument \"List.map2\""
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

let rec fold_left2 f acc l1 l2 =
  match l1, l2 with
  | [], [] -> acc
  | hd1 :: tl1, hd2 :: tl2 -> fold_left2 f (f acc hd1 hd2) tl1 tl2
  | _ :: _, [] | [], _ :: _ -> failwith "Invalid_argument \"List.fold_left2\""
;;

let rec fold_right f list acc =
  match list with
  | [] -> acc
  | hd :: tl -> f hd (fold_right f tl acc)
;;

let rec fold_right2 f l1 l2 acc =
  match l1, l2 with
  | [], [] -> acc
  | hd1 :: tl1, hd2 :: tl2 -> f hd1 hd2 (fold_right2 f tl1 tl2 acc)
  | _ :: _, [] | [], _ :: _ -> failwith "Invalid_argument \"List.fold_right2\""
;;

let rec exists f = function
  | [] -> false
  | hd :: tl -> f hd || exists f tl
;;

let rec for_all f = function
  | [] -> true
  | hd :: tl -> f hd && for_all f tl
;;

let rec find_opt p = function
  | [] -> None
  | x :: xs -> if p x then Some x else find_opt p xs
;;

let rec nth l i =
  match l with
  | [] -> failwith "Out of bounds"
  | x :: xs -> if i = 0 then x else nth xs (i - 1)
;;

let rec nth_opt l i =
  match l with
  | [] -> None
  | x :: xs -> if i = 0 then Some x else nth_opt xs (i - 1)
;;
