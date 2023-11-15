  $ noCanren -w -8 -unnesting-mode -without-activate-tactics bridge.ml2mk.ml -rectypes | ocamlformat --enable-outside-detected-project --impl -
  open GT
  open OCanren
  
  [%%distrib
  type peano = O | S of peano [@@deriving gt ~options:{ show; fmt; gmap }]]
  
  [%%distrib
  type person = A | B | C | D [@@deriving gt ~options:{ show; fmt; gmap }]]
  
  [%%distrib
  type step = One of person | Two of person * person
  [@@deriving gt ~options:{ show; fmt; gmap }]]
  
  [%%distrib
  type state = St of bool * bool * bool * bool * bool
  [@@deriving gt ~options:{ show; fmt; gmap }]]
  
  let rec greater a0 b0 q114 =
    a0 === !!O &&& (q114 === !!false)
    ||| fresh x (a0 === !!(S x))
          (b0 === !!O &&& (q114 === !!true)
          ||| fresh y (b0 === !!(S y)) (greater x y q114))
  
  let grForPerson x y q97 =
    conde
      [
        x === !!A
        &&& conde
              [
                y === !!A &&& (q97 === !!false);
                y === !!B &&& (q97 === !!true);
                y === !!C &&& (q97 === !!true);
                y === !!D &&& (q97 === !!true);
              ];
        x === !!B
        &&& conde
              [
                y === !!A &&& (q97 === !!false);
                y === !!B &&& (q97 === !!false);
                y === !!C &&& (q97 === !!false);
                y === !!D &&& (q97 === !!true);
              ];
        x === !!C
        &&& conde
              [
                y === !!A &&& (q97 === !!false);
                y === !!B &&& (q97 === !!false);
                y === !!C &&& (q97 === !!false);
                y === !!D &&& (q97 === !!true);
              ];
        x === !!D &&& (q97 === !!false);
      ]
  
  let max a0 b0 q93 =
    fresh q94 (greater a0 b0 q94)
      (conde
         [ q94 === !!true &&& (a0 === q93); q94 === !!false &&& (b0 === q93) ])
  
  let rec add a0 b0 q91 =
    a0 === !!O &&& (b0 === q91) ||| fresh x (a0 === !!(S x)) (add x !!(S b0) q91)
  
  let checkPerson state person q77 =
    fresh (l a0 b0 c0 d0)
      (state === !!(St (l, a0, b0, c0, d0)))
      (conde
         [
           person === !!A
           &&& conde
                 [ a0 === l &&& (q77 === !!true); q77 === !!false &&& (a0 =/= l) ];
           person === !!B
           &&& conde
                 [ b0 === l &&& (q77 === !!true); q77 === !!false &&& (b0 =/= l) ];
           person === !!C
           &&& conde
                 [ c0 === l &&& (q77 === !!true); q77 === !!false &&& (c0 =/= l) ];
           person === !!D
           &&& conde
                 [ d0 === l &&& (q77 === !!true); q77 === !!false &&& (d0 =/= l) ];
         ])
  
  let checkStep state step q64 =
    fresh p (step === !!(One p)) (checkPerson state p q64)
    ||| fresh (p q q65 q66 q71 q72)
          (step === !!(Two (p, q)))
          (checkPerson state p q65) (checkPerson state q q71)
          (grForPerson p q q72)
          (conde
             [
               q71 === !!false &&& (q66 === !!false);
               q71 === !!true &&& (q66 === q72);
             ])
          (conde
             [
               q65 === !!false &&& (q64 === !!false);
               q65 === !!true &&& (q64 === q66);
             ])
  
  let moveLight state q59 =
    fresh (l a0 b0 c0 d0 q60)
      (state === !!(St (l, a0, b0, c0, d0)))
      (q59 === !!(St (q60, a0, b0, c0, d0)))
      (conde
         [
           l === !!true &&& (q60 === !!false); l === !!false &&& (q60 === !!true);
         ])
  
  let movePerson state person q41 =
    fresh (l a0 b0 c0 d0)
      (state === !!(St (l, a0, b0, c0, d0)))
      (conde
         [
           fresh q43 (person === !!A)
             (q41 === !!(St (l, q43, b0, c0, d0)))
             (conde
                [
                  a0 === !!true &&& (q43 === !!false);
                  a0 === !!false &&& (q43 === !!true);
                ]);
           fresh q47 (person === !!B)
             (q41 === !!(St (l, a0, q47, c0, d0)))
             (conde
                [
                  b0 === !!true &&& (q47 === !!false);
                  b0 === !!false &&& (q47 === !!true);
                ]);
           fresh q51 (person === !!C)
             (q41 === !!(St (l, a0, b0, q51, d0)))
             (conde
                [
                  c0 === !!true &&& (q51 === !!false);
                  c0 === !!false &&& (q51 === !!true);
                ]);
           fresh q55 (person === !!D)
             (q41 === !!(St (l, a0, b0, c0, q55)))
             (conde
                [
                  d0 === !!true &&& (q55 === !!false);
                  d0 === !!false &&& (q55 === !!true);
                ]);
         ])
  
  let step state step q34 =
    fresh (p q35) (step === !!(One p)) (movePerson state p q35)
      (moveLight q35 q34)
    ||| fresh (p q q37 q39)
          (step === !!(Two (p, q)))
          (movePerson state p q39) (movePerson q39 q q37) (moveLight q37 q34)
  
  let times p q29 =
    conde
      [
        p === !!A &&& (q29 === !!(S !!O));
        p === !!B &&& (q29 === !!(S !!(S !!O)));
        p === !!C &&& (q29 === !!(S !!(S !!(S !!(S !!(S !!O))))));
        p === !!D
        &&& (q29
            === !!(S !!(S !!(S !!(S !!(S !!(S !!(S !!(S !!(S !!(S !!O)))))))))));
      ]
  
  let getTime state q25 =
    fresh p (state === !!(One p)) (times p q25)
    ||| fresh (p q q26 q27)
          (state === !!(Two (p, q)))
          (times p q26) (times q q27) (max q26 q27 q25)
  
  let getAnswer answer q1 =
    fresh (start finish)
      (start === !!(St (!!true, !!true, !!true, !!true, !!true)))
      (finish === !!(St (!!false, !!false, !!false, !!false, !!false)))
      (let rec getAnswer answer state q4 =
         fresh (x xs q6)
           (answer === !!(OCanren.Std.List.Cons (x, xs)))
           (checkStep state x q6)
           (conde
              [
                fresh (q8 q14) (q6 === !!true) (step state x q14)
                  (getAnswer xs q14 q8)
                  (q8 === !!None &&& (q4 === !!None)
                  ||| fresh (t1 q10 q12) (q8 === !!(Some t1))
                        (q4 === !!(Some q10)) (getTime x q12) (add q12 t1 q10));
                q6 === !!false &&& (q4 === !!None);
              ])
         ||| fresh q18
               (answer === !!OCanren.Std.List.Nil)
               (conde
                  [
                    state === finish &&& (q18 === !!true);
                    q18 === !!false &&& (state =/= finish);
                  ])
               (conde
                  [
                    q18 === !!true &&& (q4 === !!(Some !!O));
                    q18 === !!false &&& (q4 === !!None);
                  ])
       in
       getAnswer answer start q1)
