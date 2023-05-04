  $ ./test_run.exe
  Test (fun q -> fresh p (head_is_true p q)):
    Answer 001: true
    Answer 002: false
  
  
  Test (fun q -> fresh p (head_is_true (list Fun.id [p]) q)):
    Answer 001: true
    Answer 002: false
  
  
  Test (fun q -> fresh p (head_is_true (list Fun.id [q]) p)):
    Answer 001: true
    Answer 002: _.10 [=/= true]
  
  
  Test (fun q -> head_is_true (list (!!) []) q):
    Answer 001: false
  
  
  Test (fun q -> head_is_true (list (!!) [true]) q):
    Answer 001: true
  
  
  Test (fun q -> head_is_true (list (!!) [false]) q):
    Answer 001: false
  
  
  Test (fun q -> head_is_true q (!! true)):
    Answer 001: [true | _.12]
  
  
  Test (fun q -> head_is_true q (!! false)):
    Answer 001: _.13 [=/= [true | _.-42]]
  
  
  ======================================================================================================
  
  Test (fun q -> forall q (!! true)):
    Answer 001: []
    Answer 002: [true]
    Answer 003: [true; true]
    Answer 004: [true; true; true]
    Answer 005: [true; true; true; true]
    Answer 006: [true; true; true; true; true]
    Answer 007: [true; true; true; true; true; true]
    Answer 008: [true; true; true; true; true; true; true]
    Answer 009: [true; true; true; true; true; true; true; true]
    Answer 010: [true; true; true; true; true; true; true; true; true]
  
  
  Test (fun q -> forall q (!! false)):
    Answer 001: _.13 [=/= [true | _.-42]; =/= []]
    Answer 002: [true | _.16 [=/= [true | _.-42]; =/= []]]
    Answer 003: [true; true | _.19 [=/= [true | _.-42]; =/= []]]
    Answer 004: [true; true; true | _.22 [=/= [true | _.-42]; =/= []]]
    Answer 005: [true; true; true; true | _.25 [=/= [true | _.-42]; =/= []]]
    Answer 006: [true; true; true; true; true | _.28 [=/= [true | _.-42]; =/= []]]
    Answer 007: [true; true; true; true; true; true | _.31 [=/= [true | _.-42]; =/= []]]
    Answer 008: [true; true; true; true; true; true; true | _.34 [=/= [true | _.-42]; =/= []]]
    Answer 009: [true; true; true; true; true; true; true; true | _.37 [=/= [true | _.-42]; =/= []]]
    Answer 010: [true; true; true; true; true; true; true; true; true | _.40 [=/= [true | _.-42]; =/= []]]
  
  
  ======================================================================================================
  
  Test (fun q -> same_lens q (list (!!) []) (!! true)):
    Answer 001: []
  
  
  Test (fun q -> same_lens q (list (!!) []) (!! false)):
    Answer 001: _.12 [=/= []]
  
  
  Test (fun q -> same_lens q (list (!!) [true]) (!! true)):
    Answer 001: [_.14]
  
  
  Test (fun q -> same_lens q (list (!!) [true]) (!! false)):
    Answer 001: _.12 [=/= [_.-42 | _.-42]]
    Answer 002: [_.14 | _.20 [=/= []]]
  
  
  Test (fun q -> same_lens q (list (!!) [true; true]) (!! true)):
    Answer 001: [_.14; _.22]
  
  
  Test (fun q -> same_lens q (list (!!) [true; true]) (!! false)):
    Answer 001: _.12 [=/= [_.-42 | _.-42]]
    Answer 002: [_.14 | _.20 [=/= [_.-42 | _.-42]]]
    Answer 003: [_.14; _.22 | _.28 [=/= []]]
  
  
  Test (fun q -> fresh (a b) (q === (pair a b)) (same_lens a b (!! true))):
    Answer 001: ([], [])
    Answer 002: ([_.16], [_.18])
    Answer 003: ([_.16; _.24], [_.18; _.26])
    Answer 004: ([_.16; _.24; _.32], [_.18; _.26; _.34])
    Answer 005: ([_.16; _.24; _.32; _.40], [_.18; _.26; _.34; _.42])
    Answer 006: ([_.16; _.24; _.32; _.40; _.48], [_.18; _.26; _.34; _.42; _.50])
    Answer 007: ([_.16; _.24; _.32; _.40; _.48; _.56], [_.18; _.26; _.34; _.42; _.50; _.58])
    Answer 008: ([_.16; _.24; _.32; _.40; _.48; _.56; _.64], [_.18; _.26; _.34; _.42; _.50; _.58; _.66])
    Answer 009: ([_.16; _.24; _.32; _.40; _.48; _.56; _.64; _.72], [_.18; _.26; _.34; _.42; _.50; _.58; _.66; _.74])
    Answer 010: ([_.16; _.24; _.32; _.40; _.48; _.56; _.64; _.72; _.80], [_.18; _.26; _.34; _.42; _.50; _.58; _.66; _.74; _.82])
  
  
  Test (fun q -> fresh (a b) (q === (pair a b)) (same_lens a b (!! false))):
    Answer 001: (_.14, _.15 [=/= [_.-42 | _.-42]; =/= []])
    Answer 002: (_.14 [=/= []], _.15 [=/= [_.-42 | _.-42]])
    Answer 003: (_.14 [=/= [_.-42 | _.-42]], _.15 [=/= []])
    Answer 004: (_.14 [=/= [_.-42 | _.-42]; =/= []], _.15)
    Answer 005: ([_.16 | _.22], [_.18 | _.23 [=/= [_.-42 | _.-42]; =/= []]])
    Answer 006: ([_.16 | _.22 [=/= []]], [_.18 | _.23 [=/= [_.-42 | _.-42]]])
    Answer 007: ([_.16 | _.22 [=/= [_.-42 | _.-42]]], [_.18 | _.23 [=/= []]])
    Answer 008: ([_.16 | _.22 [=/= [_.-42 | _.-42]; =/= []]], [_.18 | _.23])
    Answer 009: ([_.16; _.24 | _.30], [_.18; _.26 | _.31 [=/= [_.-42 | _.-42]; =/= []]])
    Answer 010: ([_.16; _.24 | _.30 [=/= []]], [_.18; _.26 | _.31 [=/= [_.-42 | _.-42]]])
  
  
  ======================================================================================================
  
  Test (fun q -> eq_lists q (list (!!) []) (!! true)):
    Answer 001: []
  
  
  Test (fun q -> eq_lists q (list (!!) []) (!! false)):
    Answer 001: _.12 [=/= []]
  
  
  Test (fun q -> eq_lists q (list (!!) [true]) (!! true)):
    Answer 001: [true]
  
  
  Test (fun q -> eq_lists q (list (!!) [true]) (!! false)):
    Answer 001: _.12 [=/= [_.-42 | _.-42]]
    Answer 002: [_.19 [=/= true] | _.15]
    Answer 003: [true | _.23 [=/= []]]
  
  
  Test (fun q -> eq_lists q (list (!!) [true; true]) (!! true)):
    Answer 001: [true; true]
  
  
  Test (fun q -> eq_lists q (list (!!) [true; true]) (!! false)):
    Answer 001: _.12 [=/= [_.-42 | _.-42]]
    Answer 002: [_.19 [=/= true] | _.15]
    Answer 003: [true | _.23 [=/= [_.-42 | _.-42]]]
    Answer 004: [true; _.30 [=/= true] | _.26]
    Answer 005: [true; true | _.34 [=/= []]]
  
  
  Test (fun q -> fresh (a b) (q === (pair a b)) (eq_lists a b (!! true))):
    Answer 001: ([], [])
    Answer 002: ([_.22], [_.22])
    Answer 003: ([_.22; _.33], [_.22; _.33])
    Answer 004: ([_.22; _.33; _.44], [_.22; _.33; _.44])
    Answer 005: ([_.22; _.33; _.44; _.55], [_.22; _.33; _.44; _.55])
    Answer 006: ([_.22; _.33; _.44; _.55; _.66], [_.22; _.33; _.44; _.55; _.66])
    Answer 007: ([_.22; _.33; _.44; _.55; _.66; _.77], [_.22; _.33; _.44; _.55; _.66; _.77])
    Answer 008: ([_.22; _.33; _.44; _.55; _.66; _.77; _.88], [_.22; _.33; _.44; _.55; _.66; _.77; _.88])
    Answer 009: ([_.22; _.33; _.44; _.55; _.66; _.77; _.88; _.99], [_.22; _.33; _.44; _.55; _.66; _.77; _.88; _.99])
    Answer 010: ([_.22; _.33; _.44; _.55; _.66; _.77; _.88; _.99; _.110], [_.22; _.33; _.44; _.55; _.66; _.77; _.88; _.99; _.110])
  
  
  Test (fun q -> fresh (a b) (q === (pair a b)) (eq_lists a b (!! false))):
    Answer 001: (_.14, _.15 [=/= [_.-42 | _.-42]; =/= []])
    Answer 002: (_.14 [=/= []], _.15 [=/= [_.-42 | _.-42]])
    Answer 003: (_.14 [=/= [_.-42 | _.-42]], _.15 [=/= []])
    Answer 004: (_.14 [=/= [_.-42 | _.-42]; =/= []], _.15)
    Answer 005: ([_.21 [=/= _.22] | _.17], [_.22 | _.19])
    Answer 006: ([_.22 | _.25], [_.22 | _.26 [=/= [_.-42 | _.-42]; =/= []]])
    Answer 007: ([_.22 | _.25 [=/= []]], [_.22 | _.26 [=/= [_.-42 | _.-42]]])
    Answer 008: ([_.22 | _.25 [=/= [_.-42 | _.-42]]], [_.22 | _.26 [=/= []]])
    Answer 009: ([_.22 | _.25 [=/= [_.-42 | _.-42]; =/= []]], [_.22 | _.26])
    Answer 010: ([_.22; _.32 [=/= _.33] | _.28], [_.22; _.33 | _.30])
  
  
  ======================================================================================================
  
  Test (fun q -> matching q (!! "1")):
    Answer 001: [_.12; true; false]
  
  
  Test (fun q -> matching q (!! "2")):
    Answer 001: [_.13; false; true]
  
  
  Test (fun q -> matching q (!! "3")):
    Answer 001: [true; _.14; _.15 [=/= false; =/= true]]
    Answer 002: [true; _.14 [=/= true]; _.15 [=/= true]]
    Answer 003: [true; _.14 [=/= false]; _.15 [=/= false]]
    Answer 004: [true; _.14 [=/= false; =/= true]; _.15]
  
  
  Test (fun q -> matching q (!! "4")):
    Answer 001: [false; false; _.16 [=/= true]]
  
  
  Test (fun q -> matching q (!! "5")):
    Answer 001: [_.17 [=/= true]; _.18 [=/= false]; true]
  
  
  Test (fun q -> matching q (!! "6")):
    Answer 001: [_.19 [=/= true]; _.20 [=/= false]; _.21 [=/= false; =/= true]]
    Answer 002: [_.19 [=/= true]; _.20 [=/= false; =/= true]; _.21 [=/= true]]
    Answer 003: [_.19 [=/= false; =/= true]; _.20; _.21 [=/= false; =/= true]]
    Answer 004: [_.19 [=/= false; =/= true]; _.20 [=/= true]; _.21 [=/= true]]
  
  
  Test (fun q -> matching q (!! "7")):
    Answer 001: _.22 [=/= [_.-42; _.-42; _.-42]]
  
  
  Test (fun q -> matching q (!! "8")):
    No answers
  
  
  ======================================================================================================
  
  Test (fun q -> matching_grounded q (!! "1")):
    Answer 001: [true; true; false]
    Answer 002: [false; true; false]
  
  
  Test (fun q -> matching_grounded q (!! "2")):
    Answer 001: [true; false; true]
    Answer 002: [false; false; true]
  
  
  Test (fun q -> matching_grounded q (!! "3")):
    Answer 001: [true; true; true]
    Answer 002: [true; false; false]
  
  
  Test (fun q -> matching_grounded q (!! "4")):
    Answer 001: [false; false; false]
  
  
  Test (fun q -> matching_grounded q (!! "5")):
    Answer 001: [false; true; true]
  
  
  Test (fun q -> matching_grounded q (!! "6")):
    No answers
  
  
  Test (fun q -> matching_grounded q (!! "7")):
    No answers
  
  
  Test (fun q -> matching_grounded q (!! "8")):
    No answers
  
  
  ======================================================================================================
  

