#lang racket
(define (contar s esp cont)
  (if (< cont (string-length s))
      (contar s (if (char=? (string-ref s cont) #\ )
                    (+ esp 1)
                    esp) (+ cont 1) )
      (+ esp 1)))
(define (mostrar)
  (display "Cual es el string? ")
  (display "La cantidad de espacios es: ")
  (contar (read-line) 0 0))
(mostrar)
;Versión 2

(define (varios str ind )
  (if (char=? #\ (string-ref str (+ ind 1)) )
      (varios str (+ ind 1))
      (+ ind 1)))

(define (contard str esp ind)
  (if (< ind (string-length str))
      (if (char=? (string-ref str ind) #\ )
                    (+ esp 1)
                    esp) (varios str ind)))

(define (mostrard)
  (display "Cual es el string? ")
  (display "La cantidad de espacios es: ")
  (contard (read-line) 0 0))