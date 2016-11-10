#lang racket
(require racket/include)

(require "../../faster-miniKanren/mk.rkt")
(include "../../faster-miniKanren/test-check.scm")

(include "q.scm")

; thrines
(run 2 (x)
  (fresh (p q r)
    (=/= p q)
    (=/= q r)
    (=/= r p)
    (eval-expo p '() q)
    (eval-expo q '() r)
    (eval-expo r '() p)
    (== `(,p ,q ,r) x)))
