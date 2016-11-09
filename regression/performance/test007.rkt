#lang racket
(require racket/include)

(require "../../faster-miniKanren/mk.rkt")
(include "../../faster-miniKanren/test-check.scm")

(include "q.scm")

; quines
(run 5 (p)
  (eval-expo p '() p))
