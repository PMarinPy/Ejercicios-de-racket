#lang racket
(define (contar x n n0 n1 n2 n3 n4 n5 n6 n7 n8 n9)
  (if (>= x 1)
      (begin
        (set! n (read))
        (cond
          [(= n 1)(set! n1 (+ n1 1))]
          [(= n 2)(set! n2 (+ n2 1))]
          [(= n 3)(set! n3 (+ n3 1))]
          [(= n 4)(set! n4 (+ n4 1))]
          [(= n 5)(set! n1 (+ n5 1))]
          [(= n 6)(set! n6 (+ n6 1))]
          [(= n 7)(set! n7 (+ n7 1))]
          [(= n 8)(set! n8 (+ n8 1))]
          [(= n 9)(set! n9 (+ n9 1))]
          [(= n 0)(set! n0 (+ n0 1))])
        (contar (- x 1) n n0 n1 n2 n3 n4 n5 n6 n7 n8 n9))
      (printf "1:~a / 2:~a / 3:~a / 4:~a / 5:~a / 6:~a / 7:~a / 8:~a / 9:~a / 0:~a" n1 n2 n3 n4 n5 n6 n7 n8 n9 n0))
 )
  

(define (nums)
  (define n 0)
  (define x 0)
  (display "Función para contar la cantidad de veces que se digita cada número de una cifra.\nCúantos números digitará? ")
  (set! x (read))
  (display "Digite número por número y presione enter: ")
  (contar x n 0 0 0 0 0 0 0 0 0 0))
(nums)
  