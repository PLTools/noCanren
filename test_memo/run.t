
  $ cat > a.ml <<-EOF
  > let memo goal =
  >   let stream = OCanren.(run q) (fun q -> goal) (fun x -> x) in
  >   not (OCanren.Stream.is_empty stream)
  > 
  > let foo x =
  >   memo (OCanren.success) && x = true
  > EOF
  $ cat a.ml
  let memo goal =
    let stream = OCanren.(run q) (fun q -> goal) (fun x -> x) in
    not (OCanren.Stream.is_empty stream)
  
  let foo x =
    memo (OCanren.success) && x = true
  $ ocamlfind c -package OCanren a.ml -rectypes -linkpkg -o a.out
  $ rm -fr a.out
  $ which noCanren
  /media/mand/asp/genui/src/_build/install/default/bin/noCanren
  $ noCanren a.ml -high-order-mode -o a.out -I `ocamlfind query OCanren` -rectypes
  Got memo noCanren/src/translator.ml 484
  $ cat a.out
  open GT
  open OCanren
  open OCanren.Std
  let foo_o x_o q10 =
    fresh (q8) (OCanren.success q8)
      (conde
         [(q8 === (!! false)) &&& (q10 === (!! false));
         fresh (q3 q4) (q8 === (!! true)) (q4 === (!! true)) (x_o q3) (conde [(q3 === q4) &&& (q10 === (!! true)); (q10 === (!! false)) &&& (q3 =/= q4)])])
