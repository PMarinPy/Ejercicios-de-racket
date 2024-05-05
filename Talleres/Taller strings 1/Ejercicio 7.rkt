#lang racket
(define (esmenor str1 str2 ind min);max es el len de la cadena mas corta
  (if (< ind min)
      (if (char-ci=? (string-ref str1 ind)(string-ref str2 ind))
          (esmenor str1 str2 (+ 1 ind) min)
          (if (char-ci>? (string-ref str1 ind)(string-ref str2 ind))
              (display "La cadena uno es mayor")
              (display "La cadena dos es mayor")))
      (display "Son iguales")))
(esmenor "Hola" "Juan" 0 3)
