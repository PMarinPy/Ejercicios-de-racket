#lang racket
;PRIMER EJERICIO
(define (notac c)
  (cond
    [(char=? c #\A)(displayln "Aceptable")]
    [(char=? c #\E)(displayln "Excelente")]
    [(char=? c #\B)(displayln "Bueno")]
    [(char=? c #\D)(displayln "Deficiente")]
    (else (display "El caracter no está definido\n"))))
(define (mostrar)
  (define c 0)
  (display "Digite el caracter: ")
  (notac (read-char))
  (read-char)
  (mostrar))
(mostrar)
