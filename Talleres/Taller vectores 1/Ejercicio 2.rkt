#lang racket
(require "crear.rkt")

(define (buscar vec n c)
  (if (< c (vector-length vec))
      (if (= n (vector-ref vec c))
          c
          (buscar vec n (+ c 1)))
      -1))
(buscar (crear 20 20) 4 0)
