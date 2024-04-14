#lang racket
(require "crear.rkt")
(define (contar vec vecf cont n)

  (if (<= cont (- (vector-length vec) 1))

      (begin
         (set! n (vector-ref vec cont))
         (cond
           [(and(>= n 0)(< n 11)) (vector-set! vecf 0 (+ 1 (vector-ref vecf 0)))]
           [(and(> n 10)(< n 21)) (vector-set! vecf 1 (+ 1 (vector-ref vecf 1)))]
           [(> n 20) (vector-set! vecf 2 (+ 1 (vector-ref vecf 2)))])
         (contar vec vecf (+ cont 1) n))

       vecf))

(define (hacervec cont vec vec0 vec1 vec2 c0 c1 c2)
  (if (<= cont (- (vector-length vec) 1))

         (set! n (vector-ref vec cont))
         (cond
           [(and(>= n 0)(< n 11)) (begin
                                    (vector-set! vec0 c0 n)
                                    (hacervec vec vecf (+ cont 1) n))] 
           [(and(> n 10)(< n 21)) (vector-set! vec1 c1 (+ 1 (vector-ref vecf 1)))]
           [(> n 20) (vector-set! vec2 c2 (+ 1 (vector-ref vecf 2)))])


       


(define (mostrar)
  (define vec (crear 10 30))
  (display vec)
  (display (contar vec (vector 0 0 0) 0 0)))
 (mostrar)