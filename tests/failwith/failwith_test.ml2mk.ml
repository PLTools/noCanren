let only_true x =
  match x with
  | true -> true
  | false -> failwith "so..."
;;

let only_false x = x && only_true (failwith "oh, no...")

let is_failure x =
  match failwith "ok..." && x with
  | true -> true
  | false -> false
;;
