#lang racket
(define (decenas n)
  (cond
    ((= (quotient n 10) 1)
     (cond
       [(= n 10)"DIEZ"]
       [(= n 11)"ONCE"]
       [(= n 12)"DOCE"]
       [(= n 13)"TRECE"]
       [(= n 14)"CATORCE"]
       [(= n 15)"QUINCE"]
       [else "DIEZ"])
     
     )
    
    ((= (quotient n 10) 2) "VEINTE")
    ((= (quotient n 10) 3) "TREINTA")
    ((= (quotient n 10) 4) "CUARENTA")
    ((= (quotient n 10) 5) "CINCUENTA")
    ((= (quotient n 10) 6) "SESENTA")
    ((= (quotient n 10) 7) "SETENTA")
    ((= (quotient n 10) 8) "OCHENTA")
    ((= (quotient n 10) 9) "NOVENTA")
    ((= (quotient n 10) 0) 0)

    ))

(define (unidades n)
  (cond
    ((and (not(= n 11))(= (remainder n 10) 1)) "UNO")
    ((and (not(= n 12))(= (remainder n 10) 2)) "DOS")
    ((and (not(= n 13))(= (remainder n 10) 3)) "TRES")
    ((and (not(= n 14))(= (remainder n 10) 4)) "CUATRO")
    ((and (not(= n 15))(= (remainder n 10) 5) "CINCO"))
    ((= (remainder n 10) 6) "SEIS")
    ((= (remainder n 10) 7) "SIETE")
    ((= (remainder n 10) 8) "OCHO")
    ((= (remainder n 10) 9) "NUEVE")
    ((= (remainder n 10) 0) "")))

(define (mostrar)

  (define n 0)
  (define x 0)
  (display "Función para mostrar un numero de maximo DOS cifras en letras.\nDigite su número: ")
  (set! n (read))

  (if (and(integer? n)(< n 100)) ;Si es de 2 cifras

      (begin
        (display "¿Cúantos números desea ver? " )
        (set! x (read))
        (if (and (integer? x)(> x 0))
            (monto n x)
            (begin
              (display "Digitó mal el número.\nEjecutando de nuevo.")
              (mostrar))))

      (display "ERROR.\nEjecutando de nuevo.\n"));si no digito bien el número mostramos error
  (mostrar))


(define (monto n x)
  (if (>= x 1)
      (begin
        (if (< n 10)
            (display "")
            (begin
              (display (decenas n))
              (if (and(not(or(= n 11)(= n 12)(= n 13)(= n 14)(= n 15)))(> (remainder n 10) 0))
                  (display " Y ")
                  (display "")))
            )
        (if (and(not(or(= n 11)(= n 12)(= n 13)(= n 14)(= n 15)))(> (remainder n 10) 0)) ;Miramos si las unidades son mayores a 0

            (begin
              (displayln (unidades n)))

            (display "\n"))
        (monto (+ n 1)
               (- x 1))
        )
      (display "")))
(mostrar)