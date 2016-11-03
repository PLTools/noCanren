#lang racket
(require racket/include)

(require "mk.rkt")
(include "test-check.scm")

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
  (logo '(0 1 1 1) '(0 1) '(1 1) r))