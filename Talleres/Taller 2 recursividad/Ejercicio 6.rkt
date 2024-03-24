#lang racket
(define (maximo_digito n maximo)
  (cond
    ((= n 0) maximo) 
        (else 
          (maximo_digito (quotient n 10) 
                         (if (> (remainder n 10) maximo) 
                             (remainder n 10) 
                             maximo)))))  

(define (usar)
  (define n 0)
  (display "Funcisón para calcular el mayor número entero entre un número de varias cifras.\nDigite el número: ")
  (set! n (read))
  (maximo_digito n 0))
(usar)