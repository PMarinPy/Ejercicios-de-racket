#lang racket
(define (base n x)
  (if (> n 0)
      (begin
        (base (quotient n x) x)
        (display (remainder n x)))
      (display "")))

(define (diablo n c)
  (if (> n 0)
      (if (= (remainder n 10) 0)
          (diablo (quotient n 10) (+ c 1))
          (diablo (quotient n 10) c))
      (if (= (remainder c 2) 0)
          (display "No es diabólico")
          (display "Es diabólico"))))
(diablo 1010010 0)