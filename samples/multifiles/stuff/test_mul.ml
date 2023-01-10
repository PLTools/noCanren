let rec mul a b =
  match a with
  | Test_nat.O -> Test_nat.O
  | Test_nat.S a' -> Test_add.add b (mul a' b)
;;
