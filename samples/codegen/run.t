

  $ ../../src/noCanren.exe nat_demo.ml
  open GT
  open OCanren
  open OCanren.Std
  type 'a0 gnat =
    | O 
    | S of 'a0 
  let o () = OCanren.inj O
  let s x0 = OCanren.inj (S (x0))
  let rec add_o a_o b_o q5 =
    fresh (q1) (a_o q1)
      (((q1 === (o ())) &&& (b_o q5)) |||
         (fresh (x) (q1 === (s x))
            (add_o (fun q4 -> x === q4)
               (fun q3 -> fresh (q2) (q3 === (s q2)) (b_o q2)) q5)))
