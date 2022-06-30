
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
  $ noCanren a.ml -high-order-mode -o a.out -I `ocamlfind query OCanren` -rectypes
  $ cat a.out
  open GT
  open OCanren
  open OCanren.Std
  let foo_o x_o q11 =
    fresh (q9) (q9 === (!! true)) OCanren.success
      (conde
         [(q9 === (!! false)) &&& (q11 === (!! false));
         fresh (q4 q5) (q9 === (!! true)) (q5 === (!! true)) (x_o q4) (conde [(q4 === q5) &&& (q11 === (!! true)); (q11 === (!! false)) &&& (q4 =/= q5)])])
