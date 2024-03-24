#lang racket
(define (calculo c m n)
  (if (>= c 1)
      (begin
        (set! n (read))
        (if (> n m)
            (set! m n)
            m)
        (calculo (- c 1) m n))
      m)
      )

(define (calcularmayor)
  (define n 0)
  (define c 0)
  (define m 0)
  (display "Funcion para calcular el número mayor entre cierta cantidad.\nCúantos números evaluará: ")
  (set! c (read))
  (display "Digite número por número. (presione enter en cada digitación)\n")
  (calculo c m n))
(calcularmayor)

