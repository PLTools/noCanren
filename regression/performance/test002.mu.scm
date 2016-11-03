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

(display
 (run* (q)
   (logo '(0 1 1 1) '(0 1) '(1 1) q)))

; without display REPL prints the result but compiled code doesn't
;(display
;  (run* (q)
;    (expo '(1 1) '(1 0 1) q)))
