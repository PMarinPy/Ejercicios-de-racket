#lang racket

(define (divisores n c d)

  (if (> n 0) ;contador del caso base
      (begin

        (if (> (+ 1 (floor (/ n 2))) c );Solo miramos los numeros hasta la mitad de n, puesto que después de este no habrán divisores

            (if (= 0 (remainder n c))
                (begin
                  (displayln c)
                  (divisores n (+ c 1) (+ d 1))) ;si le residuo de entre c y n, que aumenta en 1 en cada llamado es 0, aumentamos d + 1
                (divisores n (+ c 1) d))

            (printf "Los divisores de ~a con 1 e incluyéndolo a sí mismo son: ~a" n (+ d 1)))) ;lito :)
        (display "")))
(divisores 11239899 1 0)