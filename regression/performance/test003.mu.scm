(include "../../microKanren/microKanren.scm")
(include "mini_from_microKanren.scm")

(define poso
  (lambda (n)
    (fresh (a d)
      (== `(,a . ,d) n))))

(include "numbers.scm")
(define fives
  (lambda (x)
    (disj
     (== x 5)
     (lambda (a/c)
       (lambda ()
         ((fives x) a/c))))))

(define peano
 (lambda (n)
   (conde
     ((== 'z n))
     ((fresh (n-1)
        (== `(s ,n-1) n)
        (peano n-1))))))

(define addo
 (lambda (n1 n2 out)
   (conde
     ((== 'z n1)
      (== n2 out))
     ((fresh (n1-1 res)
        (== `(s ,n1-1) n1)
        (== `(s ,res) out)
        (addo n1-1 n2 res))))))

(define <=o
 (lambda (n1 n2)
   (fresh (m)
     (addo n1 m n2))))

(define >o
 (lambda (n1 n2)
   (fresh (t zz)
          (addo n2 t n1)
          (== t `(s ,zz)))))
#|
(define >o2  ; bad
 (lambda (n1 n2)
   (conde
     ((fresh (t1)
        (== n1 `(s ,t1)) (== n2 'z))
      (fresh (t1 t2)
        (== n1 `(s ,t1)) (== n2 `(s ,t2)) (>o t1 t2))))))
|#
;(run* (q)  (<=o '(s (s z)) q))
;(run* (q)  (>o  q '(s (s z)) ))
;(run* (q)  (>o2 q '(s (s z)) ))
;(run* (q)  (<=o q '(s (s z)) ))
#|

let minmaxo a b min max = Nat.(conde [
   (min === a) &&& (max === b) &&& (a <= b);
   (max === a) &&& (min === b) &&& (a >  b)
])
|#
(define minmaxo (lambda (a b min max)
  (conde
    ((== min  a) (== max b) (<=o a b))
    ((== max  a) (== min b) (>o a b)))))

#|
(* [l] is a (non-empty) list, [s] is its smallest element,
  [l'] --- all other elements
*)
let rec smallesto l s l' = conde [
 (l === !< s) &&& (l' === !!Nil);
 fresh (h t s' t' max)
   (l' === max % t')
   (l === h % t)
   (minmaxo h s' s max)
   (smallesto t s' t')
]|#

(define smallesto (lambda (l s l2)
 (conde
   ((== l `(,s)) (== l2 '()))
   ((fresh (h t s2 t2 max)
      (== l2 `(,max . ,t2))
      (== l  `(,h   . ,t))
      (minmaxo h s2 s max)
      (smallesto t s2 t2))))))
#|
let rec sorto x y = conde [
 (* either both lists are empty *)
 (x === !!Nil) &&& (y === !!Nil);
 fresh (s xs xs')
   (* or the sorted one is a concatenation of the
      smallest element (s) and sorted list of all other elements (xs')
   *)
   (y === s % xs')
   (sorto xs xs')       (* 1 *)
   (smallesto x s xs)   (* 2 *)
]
|#
(define sorto (lambda (x y)
 (conde
    ((== x '()) (== y '()))
    ((fresh (s xs xs2)
       (== y `(,s . ,xs))
       (sorto xs xs2)
       (smallesto x s xs))))))

;(run* (q)  (<=o q '(s (s z)) ))
;(run* (q r)
;  (addo q r '(s (s z))))

;(run 1 (r q)
;  (smallesto '(
;               (s (s z))
;               (s z) z) r q))

;(run 1 (r) (sorto '() r))
;(run 1 (r) (sorto '(z) r))
(run 1 (r)
 (sorto '(
          ;(s(s(s(s(s(s(s(s (s z))))))))) ; 9
          ;(s(s(s(s(s(s(s (s z))))))))
          ;(s(s(s(s(s(s (s z)))))))
          ;(s(s(s(s(s (s z))))))          ;6
          ;(s(s(s(s (s z)))))
          ; (s(s(s (s z))))
          (s(s (s z)))                   ;3
          (s (s z))
          (s z)
          z)                             ; 0
        r))
