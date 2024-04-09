#lang racket
(define (mostrarletra caracter cont)
  (if (>= cont 1)
      (begin
        (display caracter)
        (mostrarletra caracter (- cont 1)))
      (display "")))

(define (repetirciclo fila letras espacios)
  (if (>= 6 fila)
      (begin
        (mostrarletra "A" letras)
        (mostrarletra " " espacios)
        (mostrarletra "A" letras)
        (newline)
        (repetirciclo (+ fila 1) (+ letras 1) (- espacios 2))
        )
      (display "")))

(define (repetircicloreves fila letras espacios)
  (if (>= 5 fila)
      (begin
        (mostrarletra "A" letras)
        (mostrarletra " " espacios)
        (mostrarletra "A" letras)
        (newline)
        (repetircicloreves (+ fila 1) (- letras 1) (+ espacios 2))
        )
      (display "")))

(repetirciclo 1 1 11)
(repetircicloreves 1 5 3)