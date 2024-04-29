#lang racket
;(provide vectorand crear)

(define (hacer cantidad)
  (make-vector cantidad 0))

(define (llenar vec indice max)
  (if (< indice (vector-length vec))
      (begin
        (vector-set! vec indice (random 0 (+ max 1)))
        (llenar vec (+ indice 1) max))
      void
      )
  )

(define (crear cant max)
  (define vec (hacer cant))
  (llenar vec 0 max)
  vec
   )

(define (crearmatriz vector indice cant max)
  (if (< indice cant)
     (begin
       (vector-set! vector indice (crear cant max))
       (crearmatriz vector (+ indice 1) cant max))
     vector))

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
