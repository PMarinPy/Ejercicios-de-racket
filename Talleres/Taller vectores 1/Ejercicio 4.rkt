#lang racket
(require "crear.rkt")
(define unoax (vector 1 2 3 4 5 6 7 8 9 10))

(define (invertir vec vec2 ind cont)
  (if (>= ind 0)
      (begin
        (vector-set! vec2 ind (vector-ref vec cont))
        (invertir vec vec2 (- ind 1) (+ cont 1)))
      vec2))

(define (usar)
  (define vec (crear 10 10))
  (define vec2 (vectorand 10))
  (printf "El vector a invertir es el: ~a \nEle resultado es: " vec)
  (invertir vec vec2 (- (vector-length vec) 1) 0))
(usar)