#lang racket
(require "crear.rkt")
(define (crearvec vec cont n)
  (if (> n cont)
      (begin
        (vector-set! vec cont cont)
        (crearvec vec (+ 1 cont) n))
      vec))

(define (mostrarecursi vec cont)
  (if (>= 100 cont)
      (begin
        (display (vector-ref vec (- 100 cont)))
        (display " - ")
        (mostrarecursi vec (+ cont 1)))
      (display "")))

(mostrarecursi (crearvec (vectorand 101) 0 101) 0)