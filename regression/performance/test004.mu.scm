(include "../../microKanren/microKanren.scm")
(include "mini_from_microKanren.scm")

(define poso
  (lambda (n)
    (fresh (a d)
      (== `(,a . ,d) n))))

(include "numbers.scm")

(run* (r)
  (logo '(0 0 0 0 0 0 0 0 0 0 1) '(0 1) r '(1)))
