open Maybe

let x _ = return (return "5"), return fail

let f x =
  let* x1 = x in
  let* x2 = x1 in
  return x2
;;

let g x = f fail

let h x =
  let* a, b = x in
  return a
;;
