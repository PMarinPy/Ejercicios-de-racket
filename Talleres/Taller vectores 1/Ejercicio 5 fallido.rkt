#lang racket
;# VERSIÓN CON EL DELETE siu
(define unoax (vector 1 2 3 4 5 6 7 8 9 10))

(define (puntopop vec len num ind state)

  (if  (< ind (- len 1));contador de longitud del vector
       (if state ;será ejecutado cuando sea #t, una vez encontrado el numero 
           (begin
             (vector-set! vec ind (vector-ref vec (+ ind 1)))
             (puntopop vec len num (+ ind 1) #t))
           ;se setea el valor actual con el siguiente
           (if (= (vector-ref vec ind) num)
               ;esto sera con state #f, mira si el n actual es el que estamos buscando     
               (begin
                 (vector-set! vec ind (vector-ref vec (+ ind 1)))
                 (puntopop vec len num (+ ind 1) #t)) ;state t
               (puntopop vec len num (+ ind 1) #f)));state f


       (if state ; si es t, seteamos el último con -1.
           (vector-set! vec (- len 1) -1)
           (display ""))
       )
  vec)
(puntopop unoax (vector-length unoax) 4 0 #f)

