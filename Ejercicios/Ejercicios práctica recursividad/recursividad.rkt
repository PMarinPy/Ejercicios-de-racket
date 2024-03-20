#lang racket
(define (num x y)
  (if (< x y)
      (begin
        (display "El número es: ")
        (displayln x)
        (num (+ 1 x) y))
      (if (> x y)
            (display "ERROR!\nEl primer número debe ser menor.")
            (begin
              (display "El número es: ")
              (displayln x)))))

(define (ops x y)
  (if (<= x y)
      (begin
        (display x)
        (display ": El cuadrado es: ")
        (display (expt x 2))
        (display "  --- el cubo es: ")
        (display (expt x 3))
        (display "  --- la raiz es: ")
        (display (expt x (/ 1 2)))
        (display "  --- raiz cúbica es: ")
        (displayln (expt x (/ 1 3)))
        (newline)
        (ops (+ 1 x) y))
      (if (> (- x 1) y)
            (display "ERROR!\nEl primer número debe ser menor.")
            (display "Fin"))))

(define (contar x y (c 0))
  (if (<= x y)
      (begin
        (set! c (+ c 1))
        (contar (+ 1 x) y c))
      (if (> (- x 1) y)
            (display "ERROR!\nEl primer número debe ser menor.")
            (begin
              (display "La cantidad de números es: ")
              (displayln c)))))
(contar 3 8)
