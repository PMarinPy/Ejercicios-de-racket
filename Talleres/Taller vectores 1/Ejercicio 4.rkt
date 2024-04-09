#lang racket
(define unoax (vector 1 2 3 4 5 6 7 8 9 10))

(define (invertir vec vec2 ind cont)
  (if (>= ind 0)
      (begin
        (vector-set! vec2 ind (vector-ref vec cont))
        (invertir vec vec2 (- ind 1) (+ cont 1)))
      vec2))

(invertir unoax (make-vector 10 0) (- (vector-length unoax) 1) 0)