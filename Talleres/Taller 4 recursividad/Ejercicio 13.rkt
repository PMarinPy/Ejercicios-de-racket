#lang racket
(define (cifras num cont)
  (if (> num 0)
      (cifras (quotient num 10) (+ cont 1))
      cont))
(cifras 1045900 0)