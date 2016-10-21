open MiniKanren
open Printf

let (!) = inj

let nullo q = (q === nil)
let cdro p d = fresh (fun a -> (a%d === p))

let rec build_num n =
  let rec helper = function
  | 0                   -> []
  | n -> (n mod 2) :: (helper (n/2))
  in
  LList.of_list (helper n)

(* let rec appendo a b ab ((env, subst) as st) =
  disj
    (conj (a === []) (b === ab) )
    (fresh (fun h ->
      (fresh (fun t ->
        (conj (a === h::t)
           (fresh (fun ab' ->
              conj (h::ab' === ab)
                   (appendo t b ab')
           ))
      )))
    ))
  st

let rec reverso a b ((env, subst) as st) =
  disj
    (conj (a === []) (b === []))
    (fresh (fun h ->
      (fresh (fun t ->
          (conj (a === h::t)
              (fresh (fun a' ->
                 conj (appendo a' [h] b)
                      (reverso t a')
              ))
        )
    )
  ))) st *)

(* list is not empty *)
let poso q = Fresh.two (fun h t -> q === h%t)

(* let has at least two elems *)
let gt1o q = Fresh.three (fun h t tt -> q === h%(t%tt) )

let full_addero b x y r c =
  conde [
    (!0 === b) &&& (!0 === x) &&& (!0 === y) &&& (!0 === r) &&& (!0 === c);
    (!1 === b) &&& (!0 === x) &&& (!0 === y) &&& (!1 === r) &&& (!0 === c);
    (!0 === b) &&& (!1 === x) &&& (!0 === y) &&& (!1 === r) &&& (!0 === c);
    (!1 === b) &&& (!1 === x) &&& (!0 === y) &&& (!0 === r) &&& (!1 === c);
    (!0 === b) &&& (!0 === x) &&& (!1 === y) &&& (!1 === r) &&& (!0 === c);
    (!1 === b) &&& (!0 === x) &&& (!1 === y) &&& (!0 === r) &&& (!1 === c);
    (!0 === b) &&& (!1 === x) &&& (!1 === y) &&& (!0 === r) &&& (!1 === c);
    (!1 === b) &&& (!1 === x) &&& (!1 === y) &&& (!1 === r) &&& (!1 === c);
  ]

(* let rec (?&) =
  function
  | [h] -> h
  | h :: t -> (&&&) h (?& t)
  | [] -> assert false *)

let defer (f: unit -> state -> state Stream.t) : goal =
  fun st -> Stream.from_fun (fun () -> f () st)

let rec addero d n m r =
  conde [
    (!0 === d) &&& (nil === m) &&& (n === r);
    (!0 === d) &&& (nil === n) &&& (m === r) &&& (poso m);
    (!1 === d) &&& (nil === m) &&& (defer (fun () -> addero !0 n (!< !1) r));
    (!1 === d) &&& (nil === n) &&& (poso m) &&&
      (defer (fun () -> addero !0 m (!< !1) r));
    ?& [
      ((!< !1) === n);
      ((!< !1) === m);
      Fresh.two (fun a c ->
          ((a %< c) === r) &&&
          (full_addero d !1 !1 a c)
        )
    ];
    ((!< !1) === n) &&& (gen_addero d n m r);
    ((!< !1) === m) &&& (gt1o n) &&& (gt1o r)
                    &&& (defer (fun () -> addero d (!< !1) n r));
    (gt1o n) &&& (gen_addero d n m r)
  ]
and gen_addero d n m r =
  Fresh.seven (fun a b c e x y z ->
      ?& [
          ((a % x) === n);
          ((b % y) === m);
          (poso y);
          ((c % z) === r);
          (poso z);
          (full_addero d a b c e);
          (addero e x y z)
        ])


let pluso n m k = addero !0 n m k

let minuso n m k = pluso m k n

let rec bound_multo q p n m =
  conde [
    (nullo q) &&& (poso p);
    Fresh.three (fun x y z ->
        ?& [
            (cdro q x);
            (cdro p y);
            (conde [
                 (nullo n) &&& (cdro m z) &&& (bound_multo x y z nil);
                 (cdro n z) &&& (bound_multo x y z m)
            ])
          ])
  ]

let rec multo n m p =
  conde [
    (nil === n) &&& (nil === p);
    (poso n) &&& (nil === m) &&& (nil === p);
    ((!< !1) === n) &&& (poso m) &&& (m === p);
    (gt1o n) &&& ((!< !1) === m) &&& (n === p);
    Fresh.two (fun x z ->
        ?& [
            ((!0 % x) === n);
            (poso x);
            ((!0 % z) === p);
            (poso z);
            (gt1o m);
            (multo x m z)
      ]);
    Fresh.two (fun x y ->
        ?& [
            ((!1 % x) === n);
            (poso x);
            ((!0 % y) === m);
            (poso y);
            (multo m n p);
      ]);
    Fresh.two (fun x y ->
        ?& [
            ((!1 % x) === n);
            (poso x);
            ((!1 % y) === m);
            (poso y);
            (odd_multo x n m p);
      ])
  ]
and odd_multo x n m p =
  Fresh.one (fun q ->
      (bound_multo q p n m) &&&
      (multo x m q) &&&
      (pluso (!0 % q) m p)
    )

let rec eqlo n m =
  conde [
    (nil === n) &&& (nil === m);
    ((!< !1) === n) &&& ((!< !1) === m);
    Fresh.four (fun a x b y ->
        ?& [
            ((a % x) === n);
            (poso x);
            ((b % y) === m);
            (poso y);
            (eqlo x y)
      ])
  ]

let rec ltlo n m =
  conde [
    (nil === n) &&& (poso m);
    ((!< !1) === n) &&& (gt1o m);
    Fresh.four (fun a x b y ->
        ?& [
            ((a % x) === n);
            (poso x);
            ((b % y) === m);
            (poso y);
            (ltlo x y)
      ])
  ]

let lelo n m =
  conde [
    (eqlo n m);
    (ltlo n m)
  ]

let rec lto n m =
  conde [
    (ltlo n m);
    ?& [
      (eqlo n m);
      Fresh.one (fun x ->
        (poso x) &&&
        (pluso n x m))
    ]
  ]

let leo n m =
  conde [
    (n === m);
    (lto n m)
  ]

let rec splito n r l h =
  conde [
    (nil === n) &&& (nil === h) &&& (nil === l);
    Fresh.two (fun b n' ->
        ?& [
            ((!0 % (b % n')) === n);
            (nil === r);
            ((b % n') === h);
            (nil === l)
      ]);
    Fresh.one (fun n' ->
        ?& [
           ((!1 % n') === n);
           (nil === r);
           (n' === h);
           ((!< !1) === l);
          ]);
    Fresh.four (fun b n' a r' ->
        ?& [
            ((!0 % (b % n')) === n);
            ((a % r') === r);
            (nil === l);
            (splito (b % n') r' nil h)
        ]);
    Fresh.three (fun n' a r' ->
        ?& [
           ((!1 % n') === n);
           ((a % r') === r);
           ((!< !1) === l);
           (splito n' r' nil h);
      ]);
    Fresh.five (fun b n' a r' l' ->
        ?& [
            ((b % n') === n);
            ((a % r') === r);
            ((b % l') === l);
            (poso l');
            (splito n' r' l' h)
      ]);
  ]

let rec divo n m q r =
  conde [
    (r === n) &&& (nil === q) &&& (lto n m);
    ((!< !1) === q) &&& (eqlo n m) &&& (pluso r m n) &&& (lto r m);
    ?& [
      (ltlo m n);
      (lto r m);
      (poso q);
      Fresh.eight (fun nh nl qh ql qlm qlmr rr rh ->
        (splito n r nl nh) &&&
        (splito q r ql qh) &&&
        (conde [
          (nil === nh) &&& (nil === qh) &&& (minuso nl r qlm) &&& (multo ql m qlm);
          ?& [
            (poso nh);
            (multo ql m qlm);
            (pluso qlm r qlmr);
            (minuso qlmr nl rr);
            (splito rr r nil rh);
            (divo nh m qh rh)
          ]
        ]))
    ]
  ]

let rec repeated_mul n q nq =
  conde [
    (poso n) &&& (nil === q) &&& ((!< !1) === nq);
    ((!< !1) === q) &&& (n === nq);
    ?& [
      (gt1o q);
      Fresh.two (fun q1 nq1 ->
        (pluso q1 (!< !1) q) &&&
        (repeated_mul n q1 nq1) &&&
        (multo nq1 n nq))
    ]
  ]

let rec exp2 n b q =
  conde [
    ((!< !1) === n) &&& (nil === q);
    ?& [
      (gt1o n);
      ((!< !1) === q);
      Fresh.one (fun s ->
        (splito n b s (!< !1)) )
    ];
    Fresh.two (fun q1 b2 ->
        ?& [
            ((!0 % q1) === q);
            (poso q1);
            (ltlo b n);
            (LList.appendo b (!1 % b) b2);
            (exp2 n b2 q1)
      ]);
    Fresh.four (fun q1 nh b2 s ->
        ?& [
            ((!1 % q1) === q);
            (poso q1);
            (poso nh);
            (splito n b s nh);
            (LList.appendo b (!1 % b) b2);
            (exp2 nh b2 q1)
      ])
  ]

let rec logo n b q r =
  conde [
    ((!< !1) === n) &&& (poso b) &&& (nil === q) &&& (nil === r);
    (nil === q) &&& (lto n b) &&& (pluso r (!< !1) n);
    ((!< !1) === q) &&& (gt1o b) &&& (eqlo n b) &&& (pluso r b n);
    ((!< !1) === b) &&& (poso q) &&& (pluso r (!< !1) n);
    (nil === b) &&& (poso q) &&& (r === n);
    ?& [
      ((!0 %< !1) === b);
      Fresh.three (fun a ad dd ->
          ?& [
              (poso dd);
              ((a % (ad % dd)) === n);
              (exp2 n nil q);
              (Fresh.one (fun s -> (splito n dd r s)))
              ])
    ];
    ?& [
      Fresh.four (fun a ad add ddd ->
        (conde [
          ((!1 %< !1) === b);
          ((a % (ad % (add % ddd))) === b)
        ]) &&&
      (ltlo b n) &&&
      Fresh.seven (fun bw1 bw nw nw1 ql1 ql s ->
          ?& [
        (exp2 b nil bw1);
        (pluso bw1 (!< !1) bw);
        (ltlo q n);
        (Fresh.two (fun q1 bwq1 ->
             ?& [
                 (pluso q (!< !1) q1);
                 (multo bw q1 bwq1);
                 (lto nw1 bwq1);
                 (exp2 n nil nw1);
                 (pluso nw1 (!< !1) nw);
                 (divo nw bw ql1 s);
                 (pluso ql (!< !1) ql1);
                 (lelo ql q);
                 (Fresh.five (fun bql qh s qdh qd ->
                      ?& [
                          (repeated_mul b ql bql);
                          (divo nw bw1 qh s);
                          (pluso ql qdh qh);
                          (pluso ql qd q);
                          (leo qd qdh);
                          (Fresh.three (fun bqd bq1 bq ->
                               ?& [
                                   (repeated_mul b qd bqd);
                                   (multo bql bqd bq);
                                   (multo b bq bq1);
                                   (pluso bq r n);
                                   (lto n bq1);
                          ]))
                 ]))
               ]))
            ])
      )]
    ]

let expo b q n =
  (logo n b q nil)


let rec rev_test1 f g (e,st) =
  let q, e   = Env.fresh e  in
  let r, e   = Env.fresh e  in
  let st = Subst.unify e q [] (Some st) in
  let st = Subst.unify e r [] st in
  match st with
    | None -> failwith "st is bad"
    | Some st -> LList.reverso q r (e,st)

let run_2var memo printer n goal =
  let q, e   = Env.fresh (Env.empty ())  in
  let r, e   = Env.fresh e               in
  let st     = e, Subst.empty            in
  let result = Stream.take ~n (goal q r st) in
  Printf.printf "%s {\n" memo;
  List.iter
    (fun ((env, subst) (* as st *)) ->
       (* LOG[trace1] (logn "State:"; *)
       (*              logn "%s" (show_st st); *)
       (*              logn "q=%d, r=%d" (let Var i = !! q in i) (let Var i = !!r in i)); *)
       printf "q=%s, r=%s\n" (printer env (Subst.walk' env q subst)) (printer env (Subst.walk' env r subst))
    )
    result;
  Printf.printf "}\n%!"

(* copy and pase
   Maybe we should implement more polymorphic runner like Oleg Kiselev
*)
let run_1var memo printer n goal =
  let q, e   = Env.fresh (Env.empty ())  in
  let st     = e, Subst.empty            in
  (* printf "taking %d...\n" n ; *)
  let result = Stream.take ~n (goal q st) in
  Printf.printf "%s {\n" memo;
  List.iter
    (fun ((env, subst) as _st) ->
       (* printf "WTF\n";
          printf "State:\n%!";
          printf "%s" (show_st st);
          printf "q=%d\n%!" (let Var i = !! q in i); *)
       printf "q='%s'\n" (printer env (Subst.walk' env q subst))
    )
    result;
  Printf.printf "}\n%!"


open GT

let string_of_intlist xs =
  Printf.sprintf "[%s]" (String.concat "," (List.map string_of_int xs) )

let show_int e n =
  (* logn "show_int: env=%s\n%!"  (Env.show e); *)
  string_of_int n

let rec fives x =
  disj (x === 5)
    (fun st -> Stream.from_fun (fun () -> fives x st))

let int_list e l =
  (* LOG[trace1] (logn "int_list: env=%s, list=%s\n%!"  (Env.show e)  (generic_show !!l)); *)
  show_list e show_int l


let _ =
  (* run "appendo" int_list 1 (fun q st -> appendo q [3; 4] [1; 2; 3; 4] st); *)
(*  run_2var "appendo q [] r max 1 result" int_list 1 (fun q r st -> appendo q [] r st); *)
(*   run_2var "appendo q [] r max 2 result" int_list 2 (fun q r st -> appendo q [] r st); *)
 (*  run_2var "appendo q [] r max 3 result" int_list 3 (fun q r st -> appendo q [] r st); *)
  (* run_2var "appendo q [] r max 4 result" int_list 4 (fun q r st -> appendo q [] r st); *)
(*   run_1var "reverso q [1] max 1 result" int_list 1 (fun q st -> reverso q [1; 2; 3; 4] st); *)
   (* run_1var "reverso [] [] max 1 result" int_list 1 (fun q st -> reverso [] [] st); *)
   (* run_1var "reverso [1] q max 1 result" int_list 1 (fun q st -> reverso [1; 2; 3; 4] q st); *)
   (* run_1var "rev_test1 max 1 result" int_list 1 (fun q st -> rev_test1 q q st); *)
   (* run_1var "multo" int_list 1 (fun q st -> multo (build_num 2) (build_num 3) q st); *)
  (* run_1var "fa" show_int 1 (fun q st -> full_addero 1 1 1 1 q st); *)

   (* run_1var "pluso" int_list 1 (fun q st -> pluso (build_num 0) (build_num 0) q st);
   run_1var "pluso" int_list 1 (fun q st -> pluso (build_num 2) (build_num 3) q st);
   run_1var "multo" int_list 1 (fun q st -> multo (build_num 2) (build_num 1) q st);
   run_1var "multo" int_list 1 (fun q st -> multo (build_num 2) (build_num 3) q st);

   run_1var "expo"  int_list 1 (fun q st -> expo (build_num 3) (build_num 2) q st); *)
   run_1var "expo"  int_list 1 (fun q st -> expo (build_num 3) (build_num 5) q st);
   (* run_1var "reverso q q max 1  result" int_list 1  (fun q st -> reverso q q st); *)
  (* run_1var "reverso q q max 2  result" int_list 2  (fun q st -> reverso q q st); *)
  (* run_1var "reverso q q max 3  result" int_list 3  (fun q st -> reverso q q st); *)
  (* run_1var "reverso q q max 10 result" int_list 10 (fun q st -> reverso q q st); *)

  (* run_1var "reverso q [1] max 2 results" int_list 1 (fun q st -> reverso q [1] st); *)
  (* run_1 "reverso [1] 1 max 2 results" int_list 1 (fun q st -> reverso [1] q st); *)
  (*
  run "reverso"  int_list 1  (fun q st -> reverso [1; 2; 3; 4] q st);
  run "just_a"   show_int 1  (fun q st -> just_a q st);
  run "a_and_b"  show_int 1  (fun q st -> a_and_b q st);
  run "a_and_b'" show_int 2  (fun q st -> a_and_b' q st);
  run "fives"    show_int 10 (fun q st -> fives q st)*)
  ()
