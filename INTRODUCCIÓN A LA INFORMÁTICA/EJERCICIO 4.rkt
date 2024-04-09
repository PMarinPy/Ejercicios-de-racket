#lang racket
(define (columnauno n)
  (display (- 10 n)))

(define (columnados n)
  (display (* (- n 1) 3)))

(define (columnatres x n)
  (display x
           ))
(define (mostrar x n)
  (if (<= n 10)
      (begin
        (columnauno n)
        (display " ")
        (columnados n)
        (display " ")
        (columnatres x (- 10 (* n 2)))
        (newline)
      (mostrar (+ x (- 10 (* n 2))) (+ n 1) ))
  (display "")))
(mostrar 0 1)