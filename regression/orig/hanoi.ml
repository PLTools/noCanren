open GT
open OCanren
open OCanren.Std
type 'a0 gnat =
  | Z 
  | S of 'a0 
module For_gnat = (Fmap)(struct let rec fmap fa0 = function | Z -> Z | S a0 -> S (fa0 a0)
                                type 'a0 t = 'a0 gnat end)
let rec z () = inj (For_gnat.distrib Z)
and s x__0 = inj (For_gnat.distrib (S x__0))
type gstick =
  | One 
  | Two 
  | Thr 
let one () = !! One
let two () = !! Two
let thr () = !! Thr
type 'a gtriple =
  | Triple of 'a * 'a * 'a 
module For_gtriple = (Fmap)(struct let rec fmap fa = function | Triple (a_0, a_1, a_2) -> Triple ((fa a_0), (fa a_1), (fa a_2))
                                   type 'a t = 'a gtriple end)
let rec triple x__0 x__1 x__2 = inj (For_gtriple.distrib (Triple (x__0, x__1, x__2)))
let rec less_o a_o b_o q7 =
  fresh (q1 b' q3) (q1 === (s b')) (b_o q1) (a_o q3)
    (((q3 === (z ())) &&& (q7 === (!! true))) ||| (fresh (a') (q3 === (s a')) (less_o (fun q5 -> a' === q5) (fun q6 -> b' === q6) q7)))
let get_o name_o state_o q15 =
  fresh (q9 s1 s2 s3 q11) (q9 === (triple s1 s2 s3)) (state_o q9) (name_o q11)
    (conde [(q11 === (one ())) &&& (s1 === q15); (q11 === (two ())) &&& (s2 === q15); (q11 === (thr ())) &&& (s3 === q15)])
let set_o name_o stack_o state_o q35 =
  fresh (q17 s1 s2 s3 q19) (q17 === (triple s1 s2 s3)) (state_o q17) (
    name_o q19)
    (conde
       [fresh (q20 q21 q22) (q19 === (one ())) (q35 === (triple q20 q21 q22)) (s2 === q21) (s3 === q22) (stack_o q20);
       fresh (q24 q25 q26) (q19 === (two ())) (q35 === (triple q24 q25 q26)) (s1 === q24) (s3 === q26) (stack_o q25);
       fresh (q28 q29 q30) (q19 === (thr ())) (q35 === (triple q28 q29 q30)) (s1 === q28) (s2 === q29) (stack_o q30)])
let one_step_o step_o state_o q63 =
  fresh (q64 q37 fromN toN q44 q40 q41 q46 x xs q48) (q37 === (pair fromN toN)) (
    fromN === q40) (toN === q41) (q44 === (!! true)) (q46 === (x % xs)) (
    state_o q64) (step_o q37) (conde [(q40 === q41) &&& (q44 === (!! false)); (q44 === (!! true)) &&& (q40 =/= q41)])
    (get_o (fun q61 -> fromN === q61) (fun q65 -> q65 === q64) q46) (
    get_o (fun q62 -> toN === q62) (fun q65 -> q65 === q64) q48)
    (((q48 === (nil ())) &&&
        (set_o (fun q62 -> toN === q62) (fun q50 -> fresh (q49) (q50 === (q49 % (nil ()))) (x === q49))
           (set_o (fun q61 -> fromN === q61) (fun q60 -> xs === q60) (fun q65 -> q65 === q64)) q63))
       |||
       (fresh (r y ys q52) (q48 === r) (r === (y % ys)) (q52 === (!! true)) (
          less_o (fun q59 -> x === q59) (fun q57 -> y === q57) q52)
          (set_o (fun q62 -> toN === q62) (fun q55 -> fresh (q53 q54) (q55 === (q53 % q54)) (x === q53) (r === q54))
             (set_o (fun q61 -> fromN === q61) (fun q60 -> xs === q60) (fun q65 -> q65 === q64)) q63)))
let rec check_o state_o steps_o q88 =
  fresh (q89 q67) (state_o q89) (steps_o q67)
    ((fresh (q84 q72 q73) (q67 === (nil ())) (q73 === (nil ())) (get_o (fun q68 -> q68 === (one ())) (fun q90 -> q90 === q89) q72)
        (conde [(q72 === q73) &&& (q84 === (!! true)); (q84 === (!! false)) &&& (q72 =/= q73)])
        (conde
           [(q84 === (!! false)) &&& (q88 === (!! false));
           fresh (q79 q80) (q84 === (!! true)) (q80 === (nil ())) (get_o (fun q75 -> q75 === (two ())) (fun q90 -> q90 === q89) q79)
             (conde [(q79 === q80) &&& (q88 === (!! true)); (q88 === (!! false)) &&& (q79 =/= q80)])]))
       ||| (fresh (x xs) (q67 === (x % xs)) (check_o (one_step_o (fun q86 -> x === q86) (fun q90 -> q90 === q89)) (fun q87 -> xs === q87) q88)))
let start_state_o q91 =
  q91 === (triple ((z ()) % ((s (z ())) % ((s (s (z ()))) % ((s (s (s (z ())))) % ((s (s (s (s (z ()))))) % ((s (s (s (s (s (z ())))))) % (nil ()))))))) (nil ()) (nil ()))
let answer_o q92 =
  q92 ===
    ((pair (one ()) (thr ())) %
       ((pair (one ()) (two ())) %
          ((pair (thr ()) (two ())) % ((pair (one ()) (thr ())) % ((pair (two ()) (one ())) % ((pair (two ()) (thr ())) % ((pair (one ()) (thr ())) % (nil ()))))))))
let answer'_o q93 =
  q93 ===
    ((pair (one ()) (two ())) %
       ((pair (one ()) (thr ())) %
          ((pair (two ()) (thr ())) %
             ((pair (one ()) (two ())) %
                ((pair (thr ()) (one ())) %
                   ((pair (thr ()) (two ())) %
                      ((pair (one ()) (two ())) %
                         ((pair (one ()) (thr ())) %
                            ((pair (two ()) (thr ())) %
                               ((pair (two ()) (one ())) %
                                  ((pair (thr ()) (one ())) %
                                     ((pair (two ()) (thr ())) % ((pair (one ()) (two ())) % ((pair (one ()) (thr ())) % ((pair (two ()) (thr ())) % (nil ()))))))))))))))))