#lang racket
(define (operar  m x n d)
  (if (>= n 1)
      (if (= (- m  (* 2 (- x 1)) ) 0)

          (begin
            (display "\nERROR!\nTérmino indefinido por división por denominador 0")
            (operar m (+ x 1)(- n 1) (+ (/ (- (* 2 x) 1)
                                           (- m  (* 2 (- x 1)) ) ) d)))

            (begin
              (operar m (+ x 1)(- n 1) (+ (/ (- (* 2 x) 1)
                                             (- m  (* 2 (- x 1)) ) ) (+ d (+ (/ (- (* 2 x) 1)
                                                                                (- m  (* 2 (- x 1)) ) ) d))))))
          d
          ))
  (define (mostrar)
    (define x 1)
    (define n 0)
    (define m 0)
    (define d 0)
    (display "Digite la cantidad de términos: ")
    (set! n (read))
    (display "Digite el término x: ")
    (set! m (read))
    (printf "La suma de los primeros ~a términos de la sucesión es: " n )
    (operar  m x n d))
  (mostrar)