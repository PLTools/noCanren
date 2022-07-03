open Maybe

let x = Just 5

let y = match x with
| Just a -> a
| Nothing -> 1