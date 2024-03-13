#lang racket
;recursion
(define (funcion x)
  (if (<= x 10)
      (funcion (+ x 1))
      void))