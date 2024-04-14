#lang racket
(require "crear.rkt")
(define (n_menor vec menor cont)
  (if (< cont (vector-length vec))
      (begin
        (if (= cont 0)
            (set! menor (vector-ref vec cont))
            (cond
              ((< (vector-ref vec cont) menor)
               (set! menor (vector-ref vec cont))))
            )
        (n_menor vec menor (+ 1 cont))
        )
      menor
      )
  )

(define (posicion vec menor cont)
  (if (< cont (vector-length vec))
      (if (= (vector-ref vec cont) menor)
          cont
          (posicion vec menor (+ 1 cont))
          )
      void
      )
  )

(define (n_mayor vec mayor cont)
  (if (< cont (vector-length vec))
      (begin
        (if (= cont 0)
            (set! mayor (vector-ref vec cont))
            (cond
              ((> (vector-ref vec cont) mayor)
               (set! mayor (vector-ref vec cont))))
            )
        (n_mayor vec mayor (+ 1 cont))
        )
      (+ 1 mayor)
      )
  )

(define (mostrar vec_1 vec_2 cont)
  (if (< cont (vector-length vec_1))
      (begin
       (vector-set! vec_2 cont (n_menor vec_1 0 0))
       (vector-set! vec_1 (posicion vec_1 (n_menor vec_1 0 0) 0) (n_mayor vec_1 0 0))
       (mostrar vec_1 vec_2 (+ 1 cont))
       )
      vec_2
      )
  )

(define (llamar)
  (define vec1 (crear 20 20))

  (define vec2 (vectorand 20))
  (display "El vector desorganizado es:")
  (display vec1)
  (display "Y el organizado es el:")
  (mostrar vec1 vec2 0))
(llamar)
