
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
  [@@@ocaml.warning "-8"]
  let memo goal = let stream = (let open OCanren in run q) (fun q -> goal) (fun x -> x) in not (OCanren.Stream.is_empty stream)
  let foo x = (memo OCanren.success) && (x = true)
  [@@@ocaml.warning "+8"]
  open GT
  open OCanren
  module HO =
    struct
      let foo x q11 =
        fresh (q9) (q9 === (!! true)) OCanren.success
          (conde
             [(q9 === (!! false)) &&& (q11 === (!! false));
             fresh (q4 q5) (q9 === (!! true)) (q5 === (!! true)) (x q4) (conde [(q4 === q5) &&& (q11 === (!! true)); (q11 === (!! false)) &&& (q4 =/= q5)])])
    end
  module FO = struct open HO
                     let foo q13 q12 = foo ((===) q13) q12 end
