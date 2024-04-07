#lang racket
(define (div num den rf)
  (if (>= (- num den) 0)
      (div (- num den) den (+ rf 1))
      rf))
(div 324348920 4 0)