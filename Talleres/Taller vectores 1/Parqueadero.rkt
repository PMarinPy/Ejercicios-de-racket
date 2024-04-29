#lang racket
;Posición del vector 0 = placas de los carros, 1 = hora de ingreso de los carros
(require "crear.rkt")
(define (buscarvacio vector indice)
  (if (< indice 5)
      (begin
        (if (= -1 (vector-ref (vector-ref vector indice) 0));accede a el vector de placa de la posicion cont
            indice   ;del vector principal
            (begin
              (buscarvacio vector (+ 1 indice)))
               ))
      -1))

(define (buscardato vector dato indice ph)
  (if (< indice 5)
      (if (= dato (vector-ref (vector-ref vector indice) ph))
          indice
          (buscardato vector dato (+ 1 indice) ph))
      -1))

(define (ingreso vector ocupados)
  (define indice 0)
  (define placa 0)
  (define hora 0)
  (if (< 0 (- 5 ocupados))
       (begin
         (display "Digite la placa del vehículo: ")  
         (set! placa (read))
         (display "Digite la hora de ingreso: ")
         (set! hora (read))
         (set! indice (buscarvacio vector 0))
         (setear vector placa hora indice ocupados 1)
         (printf "\nLos ocupados son ~a y el vector es ~a"ocupados vector))
       
       
       (display "No hay espacios disponibles")))

(define (salir vector ocupados)
  (define placa 0)
  (define hora 0)
  (define indice 0)
  (display "Digite la placa del vehículo: ")
  (set! placa (read))
  (set! indice (buscardato vector placa 0 0))
  (setear vector -1 -1 indice ocupados 0))

(define (setear vector placa hora indice ocupados op)
  (vector-set! (vector-ref vector indice) 0 placa)
  (vector-set! (vector-ref vector indice) 1 hora)
  (set! ocupados (if (= op 0)
                     (- ocupados 1)
                     (+ ocupados 1))))

(define (llamar vector ocupados op)
  (display "¿Qué función desea usar en su parqueadero?\n1.Ingresar un vehículo\n2.Sacar un vehículo.\n\n")
  (set! op (read))
  (display vector)
  (cond
    [(= op 1)(ingreso vector ocupados)]
    [(= op 2)(salir vector ocupados)])
  (llamar vector ocupados op))
  
(define (parqueadero)
  (define ocupados 0)
  (define vector (make-vector 5 (make-vector 2 -1)))
  (define placa 0)
  (define hora 0)
  (llamar vector ocupados 0)  
  (parqueadero))
(parqueadero)








