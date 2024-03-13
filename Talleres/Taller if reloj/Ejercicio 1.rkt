#lang racket
;funcion para bisiesto
(define (bisiesto a)
  (if (and (= 0 (remainder a 4)) (or (not(= 0 (remainder a 100))) (= 0 (remainder a 400))))
      #t
      #f))

;dias de el mes a partir de si es bisiesto y su numero AÑO Y MES
(define (dias m a)
  (if (= m 2)
      (if (bisiesto a)
          29
          28)
      (if (or (= m 4)(= m 6)(= m 9)(= m 11))
          30
          31)))

;si es el ultimo dia TRUE O FALSE y pide DIA MES Y AÑO
(define (ultd d m a)
  (if (= d (dias m a))
      true
      false)
  )
;da EL MES y pide BOOLEANO Y EL MES
;el booleano es la funcion que dice si es el último día
(define (mes d m a)
  (if (ultd d m a)
      (if (= m 12)
          1
          (+ m 1))
      m)
  )
;dice que día es
(define (dia d m a)
  (if(= d (dias m a))
     1
     (+ d 1)))
;si es el último mes
(define (ultm d m a)
  (if (and (ultd d m a)(= m 12))
      true
      false))
;Funcion para calcular el año 
(define (año d m a)
  (if (ultm d m a)
      (+ a 1)
      a))


(define (reloj)
  (define a 0)
  (define m 0)
  (define d 0)
  (define ap 0)
  (define mp 0)
  (define dp 0)
  (displayln "Función para mostrar el siguiente día.\nDigite el día: ")
  (set! d (read))
  (if (or (not(integer? d))  (> d 31)(< d 1))
      (begin
        (displayln "Por favor digite un día válido.\nEjecutando de nuevo.")
        (reloj))
      (display ""))
  (displayln "Digite el mes: ")
  (set! m (read))
  (if (or (not(integer? m))  (> m 12)(< d 1))
      (begin
        (displayln "Por favor digite un mes válido.\nEjecutando de nuevo.")
        (reloj))
      (if (> d (dias m a))
          (begin
            (display "Digito mal la cantidad de días o el mes.\nEjecutando de nuevo."))
          (display "")))
  (displayln "Digite el año: ")
  (set! a (read))
  (if (not(integer? a)  )
      (begin
        (displayln "Por favor digite un año válido.\nEjecutando de nuevo.")
        (reloj))
      (display ""))
  (set! dp (dia d m a))
  (set! mp (mes d m a))
  (set! ap (año d m a))
  (printf "Dia actual: ~a | ~a | ~a" d m a)
  (printf "\nMañana será: ~a | ~a | ~a " dp mp ap)
  )
(reloj)