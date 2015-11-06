open GT

module MiniKanren = struct
  include MiniKanren
  include MiniKanren.Make(MiniKanren.UnitLogger)
end
open MiniKanren
open Tester

@type nat = O | S of nat with show


let rec copy = function O -> O | S n -> S (copy n)

<<<<<<< HEAD


let run3 memo printer n goal =
  run (
    fresh (q r t)
      (fun st ->
        let result = take ~n:n (goal q r t st) in
        Printf.printf "%s {\n" memo;
        List.iter
          (fun st ->
             Printf.printf "q=%s, r=%s, t=%s\n" (printer st (refine st q))
                                                (printer st (refine st r))
                                                (printer st (refine st t))
          )
          result;
        Printf.printf "}\n%!"
  ))

let run2 memo printer n goal =
  run (
    fresh (q r)
      (fun st ->
        let result = take ~n:n (goal q r st) in
        Printf.printf "%s {\n" memo;
        List.iter
          (fun st ->
             Printf.printf "q=%s, r=%s\n" (printer st (refine st q)) (printer st (refine st r))
          )
          result;
        Printf.printf "}\n%!"
  ))

let run1 memo printer n goal =
  run (
    fresh (q)
      (fun st ->
        let result = take ~n:n (goal q st) in
        Printf.printf "%s {\n" memo;
        List.iter
          (fun st ->
             Printf.printf "q=%s\n" (printer st (refine st q))
          )
          result;
        Printf.printf "}\n%!"
  ))

=======
>>>>>>> 13103fd0374474dfe82b6f969f7b4790cfbbb5d6
let rec add x y =
  match x with O -> y | S n -> S (add n y)

let rec mul x y =
  match x with O -> O | S n -> add y (mul n y)

let rec addo x y z =
  conde [
    (x === O) &&& (z === y);
    fresh (x' z')
       (x === S x')
       (z === S z')
       (defer (addo x' y z'))
  ]

let rec mulo x y z =
  conde [
    (x === O) &&& (z === O);
    fresh (x' z')
      (x === S x')
      (addo y z' z)
      (defer (mulo x' y z'))
  ]

<<<<<<< HEAD
let _ =
   run1 "1 answer, addo O (S O) q"                  (mkshow nat)   1  (fun q   -> addo O (S O) q);
   run1 "1 answer, addo (S O) (S O) q"              (mkshow nat)   1  (fun q   -> addo (S O) (S O) q);
   run1 "2 answers, addo O (S O) q"                 (mkshow nat)   2  (fun q   -> addo O (S O) q);
   run1 "2 answers, addo (S O) (S O) q"             (mkshow nat)   2  (fun q   -> addo (S O) (S O) q);
   run1 "1 answer, addo q (S O) (S O)"              (mkshow nat)   1  (fun q   -> addo q (S O) (S O));
   run1 "1 answer, addo (S O) q (S O)"              (mkshow nat)   1  (fun q   -> addo (S O) q (S O));
   run1 "2 answers, addo q (S O) (S O)"             (mkshow nat)   2  (fun q   -> addo q (S O) (S O));
   run1 "2 answers, addo (S O) q (S O)"             (mkshow nat)   2  (fun q   -> addo (S O) q (S O));
   run2 "all answers, addo q r (S (S (S (S O))))"   (mkshow nat) (-1) (fun q r -> addo q r (S (S (S (S O)))));

   run1 "1 answer, mulo O (S O) q"                  (mkshow nat)   1  (fun q   -> mulo O (S O) q);
   run1 "1 answer, mulo (S (S O)) (S (S O)) q"      (mkshow nat)   1  (fun q   -> mulo (S (S O)) (S (S O)) q);
   run1 "2 answers, mulo O (S O) q"                 (mkshow nat)   2  (fun q   -> mulo O (S O) q);

   run1 "1 answer, mulo q (S (S O)) (S (S O))"      (mkshow nat)   1  (fun q   -> mulo q (S (S O)) (S (S O)));
   run1 "1 answer, mulo q (S (S O)) (S (S (S O)))"  (mkshow nat)   1  (fun q   -> mulo q (S (S O)) (S (S (S O))));
   run1 "2 answers, mulo q (S (S O)) (S (S O))"     (mkshow nat)   2  (fun q   -> mulo q (S (S O)) (S (S O)));
   run1 "2 answers, mulo q (S (S O)) (S (S (S O)))" (mkshow nat)   2  (fun q   -> mulo q (S (S O)) (S (S (S O))));

   run1 "1 answer, mulo (S (S O)) q (S (S O))"      (mkshow nat)   1  (fun q   -> mulo (S (S O)) q (S (S O)));
   run1 "1 answer, mulo (S (S O)) q (S (S (S O)))"  (mkshow nat)   1  (fun q   -> mulo (S (S O)) q (S (S (S O))));
   run1 "2 answers, mulo (S (S O)) q (S (S O))"     (mkshow nat)   2  (fun q   -> mulo (S (S O)) q (S (S O)));
   run1 "2 answers, mulo (S (S O)) q (S (S (S O)))" (mkshow nat)   2  (fun q   -> mulo (S (S O)) q (S (S (S O))));

   run2 "1 answer, mulo q (S O) r"                  (mkshow nat)   1  (fun q r -> mulo q (S O) r);
   run2 "10 answers, mulo q (S O) r"                (mkshow nat)  10  (fun q r -> mulo q (S O) r);

   run2 "1 answer, mulo (S O) q r"                  (mkshow nat)   1  (fun q r -> mulo (S O) q r);
   run2 "10 answers, mulo (S O) q r"                (mkshow nat)  10  (fun q r -> mulo (S O) q r);

   run2 "1 answer, mulo q r O"                      (mkshow nat)   1  (fun q r -> mulo q r O);
   run2 "1 answer, mulo q r (S O)"                  (mkshow nat)   1  (fun q r -> mulo q r (S O));
   run1 "1 answer, mulo (S O) (S O) q"              (mkshow nat)   1  (fun q   -> mulo (S O) (S O) q);

   run2 "1 answer, mulo q r (S (S (S (S O))))"      (mkshow nat)   1  (fun q r -> mulo q r (S (S (S (S O)))));
   run2 "3 answers, mulo q r (S (S (S (S O))))"     (mkshow nat)   3  (fun q r -> mulo q r (S (S (S (S O)))));

   run3 "1 answer, mulo q r t"                      (mkshow nat)   1  (fun q r t -> mulo q r t);
   run3 "10 answers, mulo q r t"                    (mkshow nat)   10 (fun q r t -> mulo q r t)
=======
let _ = 
  run (mkshow nat)   1    q  (fun q     st -> REPR (addo O (S O) q st)                , ["q", q]);
  run (mkshow nat)   1    q  (fun q     st -> REPR (addo (S O) (S O) q st)            , ["q", q]);
  run (mkshow nat)   2    q  (fun q     st -> REPR (addo O (S O) q st)                , ["q", q]);
  run (mkshow nat)   2    q  (fun q     st -> REPR (addo (S O) (S O) q st)            , ["q", q]);
  run (mkshow nat)   1    q  (fun q     st -> REPR (addo q (S O) (S O) st)            , ["q", q]);
  run (mkshow nat)   1    q  (fun q     st -> REPR (addo (S O) q (S O) st)            , ["q", q]);
  run (mkshow nat)   2    q  (fun q     st -> REPR (addo q (S O) (S O) st)            , ["q", q]);
  run (mkshow nat)   2    q  (fun q     st -> REPR (addo (S O) q (S O) st)            , ["q", q]);
  run (mkshow nat) (-1)  qp  (fun q p   st -> REPR (addo q p (S (S (S (S O)))) st)    , ["q", q; "p", p]);

  run (mkshow nat)   1    q  (fun q     st -> REPR (mulo O (S O) q st)                , ["q", q]);
  run (mkshow nat)   1    q  (fun q     st -> REPR (mulo (S (S O)) (S (S O)) q st)    , ["q", q]);
  run (mkshow nat)   2    q  (fun q     st -> REPR (mulo O (S O) q st)                , ["q", q]);

  run (mkshow nat)   1    q  (fun q     st -> REPR (mulo q (S (S O)) (S (S O)) st)    , ["q", q]);
  run (mkshow nat)   1    q  (fun q     st -> REPR (mulo q (S (S O)) (S (S (S O))) st), ["q", q]);
  run (mkshow nat)   2    q  (fun q     st -> REPR (mulo q (S (S O)) (S (S O)) st)    , ["q", q]);
  run (mkshow nat)   2    q  (fun q     st -> REPR (mulo q (S (S O)) (S (S (S O))) st), ["q", q]);

  run (mkshow nat)   1    q  (fun q     st -> REPR (mulo (S (S O)) q (S (S O)) st)    , ["q", q]);
  run (mkshow nat)   1    q  (fun q     st -> REPR (mulo (S (S O)) q (S (S (S O))) st), ["q", q]);
  run (mkshow nat)   2    q  (fun q     st -> REPR (mulo (S (S O)) q (S (S O)) st)    , ["q", q]);
  run (mkshow nat)   2    q  (fun q     st -> REPR (mulo (S (S O)) q (S (S (S O))) st), ["q", q]);
  
  run (mkshow nat)   1   qp  (fun q p   st -> REPR (mulo q (S O) p st)                , ["q", q; "p", p]);
  run (mkshow nat)  10   qp  (fun q p   st -> REPR (mulo q (S O) p st)                , ["q", q; "p", p]);

  run (mkshow nat)   1   qp  (fun q p   st -> REPR (mulo (S O) q p st)                , ["q", q; "p", p]);
  run (mkshow nat)  10   qp  (fun q p   st -> REPR (mulo (S O) q p st)                , ["q", q; "p", p]);

  run (mkshow nat)   1   qp  (fun q p   st -> REPR (mulo q p O st)                    , ["q", q; "p", p]);
  run (mkshow nat)   1   qp  (fun q p   st -> REPR (mulo q p (S O) st)                , ["q", q; "p", p]);
  
  run (mkshow nat)   1    q  (fun q     st -> REPR (mulo (S O) (S O) q st)            , ["q", q]);
  run (mkshow nat)   1   qp  (fun q p   st -> REPR (mulo q p (S (S (S (S O)))) st)    , ["q", q; "p", p]);
  run (mkshow nat)   3   qp  (fun q p   st -> REPR (mulo q p (S (S (S (S O)))) st)    , ["q", q; "p", p]);

  run (mkshow nat)   3  qpr  (fun q p r st -> REPR (mulo q p r st)                    , ["q", q; "p", p; "r", r]);
  run (mkshow nat)  10  qpr  (fun q p r st -> REPR (mulo q p r st)                    , ["q", q; "p", p; "r", r])
>>>>>>> 13103fd0374474dfe82b6f969f7b4790cfbbb5d6
