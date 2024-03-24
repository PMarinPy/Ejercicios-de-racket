#lang racket
(define (euclides x y)
  (if (= (remainder x y) 0)
      y
      (begin
        (euclides y (remainder x y)))))
(define (calcular)
  (define x 0)
  (define y 0)
  (display "\nFuncion para calcular el mcd mediante Euclides,\nDigite el primer numero: ")
  (set! x (read))
  (display "Digite el segundo número: ")
  (set! y (read))
  (if (> x y)
      (displayln (euclides x y))
      (displayln (euclides y x)))
  
  (calcular))
(calcular)