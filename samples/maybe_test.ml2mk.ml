open Maybe

let x = Just (Just 5), Just Nothing 

let f x = match x with
| Just (Just a) -> a
| Just (Nothing) -> 1
| Nothing -> 2

let g x = f Nothing