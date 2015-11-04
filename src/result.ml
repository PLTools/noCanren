type ('a, 'b) t = [ `Ok of 'a | `Error of 'b ]
type ('a, 'b) result = ('a, 'b) t

let return x = `Ok x
let error e = `Error e

let bind f x = match x with
  | `Ok r -> `Ok (f r)
  | err -> err
