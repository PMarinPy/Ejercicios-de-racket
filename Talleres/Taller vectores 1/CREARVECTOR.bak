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

