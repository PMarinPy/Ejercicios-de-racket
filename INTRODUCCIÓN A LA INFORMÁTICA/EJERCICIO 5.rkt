#lang racket
(define (mostrarletra caracter cont)
  (if (>= cont 1)
      (begin
        (display caracter)
        (mostrarletra caracter (- cont 1)))
      (display "")))

(define (repetirciclo fila letras espacios)
  (if (>= 7 fila)
      (begin
        (mostrarletra " " espacios)
        (mostrarletra "A" letras)
        (newline)
        (repetirciclo (+ fila 1) (- letras 2) (+ espacios 1)))
      (display "")))
(repetirciclo 1 13 0)