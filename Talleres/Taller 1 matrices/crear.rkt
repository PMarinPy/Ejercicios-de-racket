#lang racket
(provide vectorand crear crearord)

(define (vectorand cant)
  (make-vector cant 0))

(define (orgvector v p n)
  (if (< p (vector-length v))
      (begin
        (vector-set! v p (random 0 (+ n 1)))
        (orgvector v (+ p 1) n))
      void
      )
  )
(define (orgvectormax v p n nmax)
  (if (< p (vector-length v))
      (begin
        (vector-set! v p (random n nmax))
        (orgvectormax v (+ p 1) n nmax))
      void
      )
  )
(define (crear cant n)
  (define vec (vectorand cant))
  (orgvector vec 0 n)
  vec
   )

(define (crearord cant n nmax)
  (define vec (vectorand cant))
  (orgvectormax vec 0 n nmax)
  vec
   )