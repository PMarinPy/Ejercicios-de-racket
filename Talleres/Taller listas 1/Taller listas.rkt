#lang racket
(define (length lista len)
  (if (null? lista)
      len
      (length (cdr lista) (+ len 1))))
(define (find dato lista cont)
  (if (not(null? lista))
      (if (= dato (car lista))
          cont
          (find dato (cdr lista) (+ cont 1)))
      -1))
;EJERCICIO UNO
(define (times dato lista cont)
  (if (not(null? lista))
      (if (= (car lista) dato)
          (times dato (cdr lista) (+ cont 1))
          (times dato (cdr lista) cont))
      cont))
;EJERCICIO DOS
