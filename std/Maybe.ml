type 'a maybe = Nothing | Just of 'a 

let ( let* ) x f = match x with
| Just a -> f a
| Nothing -> Nothing
