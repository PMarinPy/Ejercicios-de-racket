#lang racket
(require "crear.rkt")
(define (encontrar vec num ind)
  (if  (< ind (- (vector-length vec) 1));contador de longitud del vector
           (if (= (vector-ref vec ind) num)
               ind
               (encontrar vec num (+ ind 1)))
           -1
               ))
(define (mover vec cont)
  (if (< cont (- (vector-length vec) 1))
      (begin
        (vector-set! vec cont (vector-ref vec (+ cont 1)))
        (mover vec (+ cont 1)))
      (vector-set! vec (- (vector-length vec) 1) -1)
        )
  vec)
(define (usar)
  (define num 0)
  (define vec (crear 20 20))
  (define pos 0)
  (display "Función para encontrar un número en un vector, digite el número a encontrar: ")
  (set! num (read))
  (set! pos (encontrar vec num 0))
  (if (not(= -1 pos))
      (begin
        (printf "El vector es: ~a y con el número borrado es:" vec )
        (displayln (mover vec pos)))
      (printf "No se econtró el número, el vector es: ~a\n" vec))
  (usar))
(usar)