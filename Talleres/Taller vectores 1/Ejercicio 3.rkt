#lang racket
(require "crear.rkt")

(define (contar vec n c cant)
  (if (< c (vector-length vec))
      (if (= n (vector-ref vec c))
          (contar vec n (+ c 1) (+ cant 1))
          (contar vec n (+ c 1) cant))
      cant))

(contar (crear 20 10) 4 0 0)