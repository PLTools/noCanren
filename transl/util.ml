let mangle_construct_name name =
  let low = String.mapi (function 0 -> Char.lowercase | _ -> fun x -> x ) name in
  match low with
  | "val" | "if" | "else" | "for" | "do" | "let" | "open" | "not" | "pair" | "conj" -> low ^ "_"
  | _ -> low
