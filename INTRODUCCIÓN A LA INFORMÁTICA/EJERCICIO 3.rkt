#lang racket
(define (mostrar n)
  (if (< 0 n)
      (begin
        (mostrar (- n 1))
        (displayln n))
      (display "")))
(mostrar 10)