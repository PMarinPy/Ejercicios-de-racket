#lang racket
(define (multiplicacion num num2 rf)
  (if (> num 0)
      (multiplicacion (- num 1) num2 (+ rf num2))
      rf))
(multiplicacion 12 10 0)