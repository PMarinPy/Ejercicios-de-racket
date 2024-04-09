#lang racket
(define (esprimo n c)
  (if (< c (+ 1 (floor (/ n 2))))
      (if (= 0 (remainder n c))
          #f
          (esprimo n (+ c 1)))
      #t))