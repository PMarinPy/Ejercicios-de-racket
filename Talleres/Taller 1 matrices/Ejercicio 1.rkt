#lang racket
(require "crear.rkt")

(define (llenarvec vector cont cols)

  (if (< cont (vector-length vector))
      (begin
        (vector-set! vector cont (crearord cols 10 99))
        (llenarvec vector (+ cont 1) cols))
      void))

(define (mostrar vector contfil contcol)
  (if (< contfil (vector-length vector))
      (if (< contcol (vector-length (vector-ref vector 0)))
          (begin
            (display (vector-ref (vector-ref vector contfil) contcol))
            (display " ")
            (mostrar vector contfil (+ contcol 1)))
          (begin

            (newline)           
            (mostrar vector (+ contfil 1) 0)))
      (display "")))

(define (sumarfilas vector contfil contcol sum)
  (if (< contfil (vector-length vector))
      (if (< contcol (vector-length (vector-ref vector 0)))
          (begin
            (sumarfilas vector contfil (+ contcol 1) (+ sum (vector-ref (vector-ref vector contfil) contcol))))
          (begin
            (printf "\nLa suma de la fila ~a da como resultado ~a\n" contfil sum)           
            (sumarfilas vector (+ contfil 1) 0 0)))
      (display "")))

(define (promedio vector cols contfil contcol sum)
  (if (< contcol cols)
      (if (< contfil (vector-length vector))
          (begin
            (promedio vector cols (+ contfil 1) contcol (+ sum (vector-ref (vector-ref vector contfil) contcol))))
          (begin
            (printf "\nEl promedio de la columna ~a da como resultado ~a\n" contcol (round (/ sum cols)))           
            (promedio vector cols 0 (+ contcol 1) 0)))
      (display "")))

(define (sumarcols vector cols contfil contcol sum)
  (if (< contcol cols)
      (if (< contfil (vector-length vector))
          (begin
            (sumarcols vector cols (+ contfil 1) contcol (+ sum (vector-ref (vector-ref vector contfil) contcol))))
          (begin
            (printf "\nLa suma la columna ~a da como resultado ~a\n" contcol sum)           
            (sumarcols vector cols 0 (+ contcol 1) 0)))
      (display "")))


(define (pedir)
  (define vector 0)
  (define filas 0)
  (define cols 0)
  (display "Cantidad de filas: ")
  (set! filas (read))
  (display "Cantidad de columnas: ")
  (set! cols (read))
  (set! vector (make-vector filas 0))
  (llenarvec vector 0 cols)
  (display "\nEl vector es: \n")
  (mostrar vector 0 0)
  (sumarfilas vector 0 0 0)
  (newline)
  (display "-------------------------")
  (newline)
  (promedio vector cols 0 0 0))

(pedir)