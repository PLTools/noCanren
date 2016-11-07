#lang racket
(require racket/include)

(require "../../faster-miniKanren/mk.rkt")
(include "../../faster-miniKanren/test-check.scm")

(define nullo
  (lambda (x)
    (== '() x)))
(define pairo
  (lambda (p)
    (fresh (a d)
      (conso a d p))))
(define conso
  (lambda (a d p)
    (== (cons a d) p)))
(define poso
  (lambda (n)
    (fresh (a d)
      (== `(,a . ,d) n))))
(define cdro
  (lambda (p d)
    (fresh (a)
      (== (cons a d) p))))
(define caro
  (lambda (p a)
    (fresh (d)
      (== (cons a d) p))))

(include "numbers.scm")

(run* (r)
  (logo '(0 0 0 0 0 0 0 0 0 0 1) '(0 1) r '(1)))
