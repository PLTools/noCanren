

  $ ../../src/noCanren.exe nat_demo.ml
  [@@@ocaml.warning "-8"]
  type nat =
    | O 
    | S of nat 
  let rec add a b = match a with | O -> b | S x -> add x (S b)
  [@@@ocaml.warning "+8"]
  open GT
  open OCanren
  module HO =
    struct
      [%%distrib
        type nat =
          | O 
          | S of nat [@@deriving gt ~options:{ show; fmt; gmap }]]
      let rec add a b q5 =
        fresh (q1) (a q1)
          (((q1 === (!! O)) &&& (b q5)) |||
             (fresh (x) (q1 === (!! (S x)))
                (add (fun q4 -> x === q4)
                   (fun q3 -> fresh (q2) (q3 === (!! (S q2))) (b q2)) q5)))
    end
  module FO = struct open HO
                     let add q8 q7 q6 = add ((===) q8) ((===) q7) q6 end
