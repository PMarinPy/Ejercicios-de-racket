#lang racket
(define (maximo_digito n m c i)
  (cond ((= n 0) (printf "El mayor número es el: ~a y está en el índice: ~a" m i)) 
        (else 
          (maximo_digito (quotient n 10) 
                         (if (> (remainder n 10) m) 
                             (remainder n 10) 
                             m)
                         (+ c 1)
                         (if (> (remainder n 10) m) 
                             (+ c 1)
                             i)))))


(define (usar)
  (define n 0)
  (display "Función para retornar el índice y el número más grande de otro número.\nDigite el número: ")
  (set! n (read))
  (maximo_digito n 0 0 0))
(usar)