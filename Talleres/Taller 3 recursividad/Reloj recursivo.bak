#lang racket
;Funcion para sumar minuto y mostrarlo
(define (segundos s)
  (if (= s 59)
      0
      0))
(define (minuto m s)
  (if  (= 59 m)
       0
       (+ m 1)))
(define (hora h m s)
  (if (and (not(= h 23)) (= m 59))
      (+ h 1)
      (if (and(= h 23)(= m 59))
          00
          h)
      ))

(define (mostrar)
  (define h 0)
  (define m 0)
  (define s 0)
  (define n 0)
  (define hp 0)
  (define mp 0)
  (define sp 0)
  (display "Función para calcular el segundo siguiente.\nDigite la hora: ")
  ;Seteamos las h y prevenimos error
  (set! h (read))
  (if (or (not(integer? h)) (< h 0) (> h 23))
      (begin (display "Error en la digitación.\nEjecutando de nuevo.\n")(mostrar))
      (display ""))
  (display "Digite los minutos: ")
  ;Seteamos los m y prevenimos error
  (set! m (read))
  (if (or(not(integer? m)) (< m 0) (> m 59))
      (begin (display "Error en la digitación.\nEjecutando de nuevo.")(mostrar))
      (display ""))
  (display "Digite los segundos: ")
  ;Seteamos los s y prevenimos error
  (set! s (read))
  (if (or(not(integer? s))(< s 0)(> s 59))
      (begin (display "Error en la digitación.\nEjecutando de nuevo.")(mostrar))
      (display ""))
  (display "¿Cúantos minutos desea mostrar? ")
  (set! n (read))
  (if (and(> n 0)(integer? n))
        (recursivito n h m s)
        (begin
          (display "Digitó mal el número.\nEjecutando de nuevo.")
          (mostrar))
        )

  (mostrar)
  )

(define (recursivito n h m s)
  (if (>= n 1)
      (begin
        (printf "~a:~a:~a\n" (hora h m s) (minuto m s) (segundos s))
        (recursivito (- n 1 ) (hora h m s) (minuto m s) (segundos s)))
      (display "")))

(mostrar)