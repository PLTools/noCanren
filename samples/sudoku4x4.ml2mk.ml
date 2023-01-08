type num =
  | N1
  | N2
  | N3
  | N4

type sudoku4X4 =
  | S4x4 of
      num
      * num
      * num
      * num
      * num
      * num
      * num
      * num
      * num
      * num
      * num
      * num
      * num
      * num
      * num
      * num

let not_eq a b =
  match a with
  | N1 ->
    (match b with
     | N1 -> false
     | N2 -> true
     | N3 -> true
     | N4 -> true)
  | N2 ->
    (match b with
     | N1 -> true
     | N2 -> false
     | N3 -> true
     | N4 -> true)
  | N3 ->
    (match b with
     | N1 -> true
     | N2 -> true
     | N3 -> false
     | N4 -> true)
  | N4 ->
    (match b with
     | N1 -> true
     | N2 -> true
     | N3 -> true
     | N4 -> false)
;;

let all_different a b c d =
  not_eq a b && not_eq a c && not_eq a d && not_eq b c && not_eq b d && not_eq c d
;;

let check_sudoku sudoku =
  match sudoku with
  | S4x4 (a11, a12, a13, a14, a21, a22, a23, a24, a31, a32, a33, a34, a41, a42, a43, a44)
    ->
    all_different a11 a12 a13 a14
    && all_different a21 a22 a23 a24
    && all_different a31 a32 a33 a34
    && all_different a41 a42 a43 a44
    && all_different a11 a21 a31 a41
    && all_different a12 a22 a32 a42
    && all_different a13 a23 a33 a43
    && all_different a14 a24 a34 a44
    && all_different a11 a12 a21 a22
    && all_different a13 a14 a23 a24
    && all_different a31 a32 a41 a42
    && all_different a33 a34 a43 a44
;;
