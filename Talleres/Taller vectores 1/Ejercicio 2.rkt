#lang racket
(define (buscar vec n c)
  (if (< c (vector-length vec))
      (if (= n (vector-ref vec c))
          (- c 1)
          (buscar vec n (+ c 1)))
      -1))
(define unoax (vector 10 1 4 1 3 5 5 6 7 8 9 10))
(buscar unoax 4 0)