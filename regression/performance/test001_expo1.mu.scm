(include "../../microKanren/microKanren.scm")
(include "../../microKanren/miniKanren-wrappers2.scm")


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
   (expo '(1 1) '(1 0 1) q)))

; without display REPL prints the result but compiled code doesn't
;(display
;  (run* (q)
;    (expo '(1 1) '(1 0 1) q)))
