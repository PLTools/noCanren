

  $ ../../src/noCanren.exe nat_demo.ml
  [@@@ocaml.warning "-8"]
  type nat =
    | O 
    | S of nat 
  let rec add a b = match a with | O -> b | S x -> add x (S b)
  [@@@ocaml.warning "+8"]
  open GT
  open OCanren
  open OCanren.Std
  module HO =
    struct
      type 'self gnat =
        | O 
        | S of 'self 
      let o () = OCanren.inj O
      let s x0 = OCanren.inj (S (x0))
      let rec add_o a_o b_o q5 =
        fresh (q1) (a_o q1)
          (((q1 === (!! O)) &&& (b_o q5)) |||
             (fresh (x) (q1 === (!! (S x)))
                (add_o (fun q4 -> x === q4)
                   (fun q3 -> fresh (q2) (q3 === (!! (S q2))) (b_o q2)) q5)))
    end
  module FO =
    struct open HO
           let add q8 q7 q6 = add_o ((===) q8) ((===) q7) q6 end
