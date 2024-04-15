#lang racket
(require "crear.rkt")
(define espacio 50)

(define (buscarplaca placa indice vecplacas espacio)
  (if (< indice espacio)
      (if (= (vector-ref vecplacas indice) placa)
          indice
          (buscarplaca placa (+ indice 1) vecplacas espacio))
      -1))

(define (desocupado vecplacas contador espacio)
  (if (< contador espacio)
      (if (= -1 (vector-ref vecplacas contador))
          contador
          (desocupado vecplacas (+ contador 1)))
      -1))

(define (ingreso vecplacas vechoras contador espacio)
  (if (<= contador espacio)
      (begin
        (display "Por favor digite la placa de el auto: ")
        (vector-set! vecplacas (desocupado vecplacas 0) (read))
        (display "Por favor digite la hora de ingreso: ")
        ((vector-set! vechoras )(desocupado vechoras 0) (read)))
      (displayln "No es posible parquear, está lleno.")))

(define (salida vecplacas vechoras contador)
  (define indice 0)
  (define h 0)
  (display "Por favor digite la placa: ")
  (set! indice (buscarplaca (read) 0 vecplacas espacio))
  (display "Hora de salida: ")
  (set! h (read))
  (display "Total a pagar: ")
  (display (* 1800 (- (h) (vector-ref vechoras indice))))
  (displayln "Que pase un buen día")
  (vector-set! vecplacas indice -1)
  (vector-set! vechoras indice -1))

(define (horastotales vechoras buscarplaca)
  (define placa 0)
  (define h 0)
  (display "Digite la placa del vehículo: ")
  (set! placa (read))
  (display "¿Qué hora es? ")
  (set! h 0)
  


  )



















