#lang racket
(require "crear.rkt")
(define (promedio vec sum cont)
  (if (<= cont (- (vector-length vec) 1))
      (promedio vec (+ sum (vector-ref vec cont)) (+ cont 1))
      (/ sum (vector-length vec))))
(define (usar)
  (define vec (crear 20 20))
  (printf "El vector creado es el ~a y su promedio es: " vec)
  (round(promedio vec 0 0)))
(usar)

