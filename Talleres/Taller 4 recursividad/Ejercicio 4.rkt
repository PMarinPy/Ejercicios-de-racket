#lang racket
(define (esprimo n c)
  (if (< c (+ 1 (floor (/ n 2))))
      (if (= 0 (remainder n c))
          #f
          (esprimo n (+ c 1)))
      #t))

(define (hallarprimos n c)
  (cond
    [(= n 0) (displayln "Finalizando búsqueda de primos.")]
    [(esprimo c 2)
     (begin
       (printf "El ~a es primo.\n" c)
       (hallarprimos (- n 1) (+ c 1)))]
    [else
     (hallarprimos n (+ c 1))]))

(define (ppal)
  (display "Ingrese la cantidad de números primos que desea encontrar: ")
  (define n (read))
  (display "Los números primos son:\n")
  (hallarprimos n 2))

(ppal)
