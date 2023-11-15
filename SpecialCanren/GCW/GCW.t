  $ noCanren -w -8 -unnesting-mode -without-activate-tactics GCW.ml2mk.ml -rectypes | ocamlformat --enable-outside-detected-project --impl -
  open GT
  open OCanren
  
  [%%distrib
  type person = G | C | W | N [@@deriving gt ~options:{ show; fmt; gmap }]]
  
  [%%distrib
  type state = St of bool * bool * bool * bool
  [@@deriving gt ~options:{ show; fmt; gmap }]]
  
  let checkState s q55 =
    fresh (i0 g0 c0 w0 q57)
      (s === !!(St (i0, g0, c0, w0)))
      (conde [ i0 === g0 &&& (q57 === !!true); q57 === !!false &&& (i0 =/= g0) ])
      (conde
         [
           q57 === !!true &&& (q55 === !!true);
           fresh q60 (q57 === !!false)
             (conde
                [
                  i0 === c0 &&& (q60 === !!true); q60 === !!false &&& (i0 =/= c0);
                ])
             (conde
                [
                  q60 === !!true
                  &&& conde
                        [
                          i0 === w0 &&& (q55 === !!true);
                          q55 === !!false &&& (i0 =/= w0);
                        ];
                  q60 === !!false &&& (q55 === !!false);
                ]);
         ])
  
  let checkStep state step q43 =
    fresh (i0 g0 c0 w0)
      (state === !!(St (i0, g0, c0, w0)))
      (conde
         [
           step === !!N &&& (q43 === !!true);
           step === !!G
           &&& conde
                 [
                   i0 === g0 &&& (q43 === !!true); q43 === !!false &&& (i0 =/= g0);
                 ];
           step === !!C
           &&& conde
                 [
                   i0 === c0 &&& (q43 === !!true); q43 === !!false &&& (i0 =/= c0);
                 ];
           step === !!W
           &&& conde
                 [
                   i0 === w0 &&& (q43 === !!true); q43 === !!false &&& (i0 =/= w0);
                 ];
         ])
  
  let step s p q16 =
    fresh (i0 g0 c0 w0)
      (s === !!(St (i0, g0, c0, w0)))
      (conde
         [
           fresh (q18 q19) (p === !!G)
             (q16 === !!(St (q18, q19, c0, w0)))
             (conde
                [
                  i0 === !!true &&& (q18 === !!false);
                  i0 === !!false &&& (q18 === !!true);
                ])
             (conde
                [
                  g0 === !!true &&& (q19 === !!false);
                  g0 === !!false &&& (q19 === !!true);
                ]);
           fresh (q25 q26) (p === !!C)
             (q16 === !!(St (q25, g0, q26, w0)))
             (conde
                [
                  i0 === !!true &&& (q25 === !!false);
                  i0 === !!false &&& (q25 === !!true);
                ])
             (conde
                [
                  c0 === !!true &&& (q26 === !!false);
                  c0 === !!false &&& (q26 === !!true);
                ]);
           fresh (q32 q33) (p === !!W)
             (q16 === !!(St (q32, g0, c0, q33)))
             (conde
                [
                  i0 === !!true &&& (q32 === !!false);
                  i0 === !!false &&& (q32 === !!true);
                ])
             (conde
                [
                  w0 === !!true &&& (q33 === !!false);
                  w0 === !!false &&& (q33 === !!true);
                ]);
           fresh q39 (p === !!N)
             (q16 === !!(St (q39, g0, c0, w0)))
             (conde
                [
                  i0 === !!true &&& (q39 === !!false);
                  i0 === !!false &&& (q39 === !!true);
                ]);
         ])
  
  let checkAnswer a q1 =
    fresh (startState finishState)
      (startState === !!(St (!!true, !!true, !!true, !!true)))
      (finishState === !!(St (!!false, !!false, !!false, !!false)))
      (let rec checkAnswer a state q4 =
         a === !!OCanren.Std.List.Nil
         &&& conde
               [
                 state === finishState &&& (q4 === !!true);
                 q4 === !!false &&& (state =/= finishState);
               ]
         ||| fresh (x xs q9)
               (a === !!(OCanren.Std.List.Cons (x, xs)))
               (checkStep state x q9)
               (conde
                  [
                    fresh (newState q12) (q9 === !!true) (step state x newState)
                      (checkState newState q12)
                      (conde
                         [
                           q12 === !!true &&& checkAnswer xs newState q4;
                           q12 === !!false &&& (q4 === !!false);
                         ]);
                    q9 === !!false &&& (q4 === !!false);
                  ])
       in
       checkAnswer a startState q1)
