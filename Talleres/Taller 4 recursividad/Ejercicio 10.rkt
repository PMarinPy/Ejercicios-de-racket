#lang racket
(define (factorial num rf)
  (if (= num 0)
      rf
      (if (= 1 num)
          rf
          (factorial (- num 1) (* rf num)))))
(factorial 0 1)