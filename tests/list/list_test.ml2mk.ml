open List
open Peano

let is_5 = length [ 1; 2; 3; 4; 5 ]
let is_10 = fold_left ( + ) 0 [ 1; 2; 3; 4 ]
let is_15 = fold_right ( + ) [ 1; 2; 3; 4; 5 ] 0
let key_value_list = [ 1, 10; 2, 20; 3, 30 ]
let is_20 = assoc 1 key_value_list
let fail () = assoc 4 key_value_list
let assoc = assoc
let mul = fold_left ( * ) 1
let dot_product x y = fold_right2 (fun x y acc -> acc + (x * y)) x y 0
let dot_product_with_indexes l = fold_left ( + ) 0 @@ mapi (fun i n -> i * n) l
