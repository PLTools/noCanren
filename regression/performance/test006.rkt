#lang racket
(require racket/include)

(require "../../faster-miniKanren/mk.rkt")
(include "../../faster-miniKanren/test-check.scm")

(include "q.scm")

; twines
(run 2 (x)
  (fresh (p q)
    (=/= p q)
    (eval-expo p '() q)
    (eval-expo q '() p)
    (== `(,p ,q) x)))
