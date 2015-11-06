open GT
open MiniKanren
<<<<<<< HEAD
module M = MiniKanren.Make(MiniKanren.UnitLogger)
open M

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
open Tester
>>>>>>> 13103fd0374474dfe82b6f969f7b4790cfbbb5d6

let just_a a = a === 5

let a_and_b a =
  fresh (b)
    (a === 7)
    (conde [b === 6; b === 5])

let a_and_b' b =
  fresh (a)
    (a === 7)
    (conde [b === 6; b === 5])

let rec fives x = (x === 5) ||| defer (fives x)

let rec appendo a b ab =
  conde [
    (a === []) &&& (b === ab);
    fresh (h t ab')
      (a === h::t)
      (h::ab' === ab)
      (appendo t b ab')
  ]

let rec reverso a b =
  conde [
    (a === []) &&& (b === []);
    fresh (h t a')
      (a === h::t)
      (appendo a' [h] b)
      (reverso t a')
  ]

let int_list st l = mkshow(list) (mkshow(int)) st l

let _ =
<<<<<<< HEAD
   run1 "appendo q [3; 4] [1; 2; 3; 4] max 1 result" int_list       1 (fun q   -> appendo q [3; 4] [1; 2; 3; 4]);
   run2 "appendo q [] r max 4 results"               int_list       4 (fun q r -> appendo q [] r);
   run1 "reverso q [1; 2; 3; 4] max 1 result"        int_list       1 (fun q   -> reverso q [1; 2; 3; 4]);
   run1 "reverso [] [] max 1 result"                 int_list       1 (fun q   -> reverso [] []);
   run1 "reverso [1; 2; 3; 4] q max 1 result"        int_list       1 (fun q   -> reverso [1; 2; 3; 4] q);
   run1 "reverso q q max 1 result"                   int_list       1 (fun q   -> reverso q q);
   run1 "reverso q q max 2 result"                   int_list       2 (fun q   -> reverso q q);
   run1 "reverso q q max 3 result"                   int_list       3 (fun q   -> reverso q q);
   run1 "reverso q q max 10 results"                 int_list      10 (fun q   -> reverso q q);
   run1 "reverso q [1] max 2 results"                int_list       2 (fun q   -> reverso q [1]);
   run1 "reverso [1] q max 2 results"                int_list       1 (fun q   -> reverso [1] q);
   run1 "just_a"                                     (mkshow(int))  1 (fun q   -> just_a q);
   run1 "a_and_b"                                    (mkshow(int))  1 (fun q   -> a_and_b q);
   run1 "a_and_b'"                                   (mkshow(int))  2 (fun q   -> a_and_b' q);
   run1 "fives"                                      (mkshow(int)) 10 (fun q   -> fives q)
=======
  run int_list       1 q  (fun q   st -> REPR (appendo q [3; 4] [1; 2; 3; 4] st), ["q", q]);
  run int_list       4 qp (fun q p st -> REPR (appendo q [] p st)               , ["q", q; "p", p]);
  run int_list       1 q  (fun q   st -> REPR (reverso q [1; 2; 3; 4] st)       , ["q", q]);
  run int_list       1 q  (fun q   st -> REPR (reverso [] [] st)                , ["q", q]);
  run int_list       1 q  (fun q   st -> REPR (reverso [1; 2; 3; 4] q st)       , ["q", q]);
  run int_list       1 q  (fun q   st -> REPR (reverso q q st)                  , ["q", q]);
  run int_list       2 q  (fun q   st -> REPR (reverso q q st)                  , ["q", q]);
  run int_list       3 q  (fun q   st -> REPR (reverso q q st)                  , ["q", q]);
  run int_list      10 q  (fun q   st -> REPR (reverso q q st)                  , ["q", q]);
  run int_list       2 q  (fun q   st -> REPR (reverso q [1] st)                , ["q", q]);
  run int_list       1 q  (fun q   st -> REPR (reverso [1] q st)                , ["q", q]);
  run (mkshow(int))  1 q  (fun q   st -> REPR (a_and_b q st)                    , ["q", q]); 
  run (mkshow(int))  2 q  (fun q   st -> REPR (a_and_b' q st)                   , ["q", q]); 
  run (mkshow(int)) 10 q  (fun q   st -> REPR (fives q st)                      , ["q", q])
>>>>>>> 13103fd0374474dfe82b6f969f7b4790cfbbb5d6
