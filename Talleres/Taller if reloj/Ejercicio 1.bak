#lang racket
(define (bisiesto a)
  (if (and (= 0 (remainder a 4)) (or (not(= 0 (remainder a 100))) (= 0 (remainder a 400))))
      #t
      #f))


(define (dias m a)
  (if (= m 2)
      (if (bisiesto a)
          29
          28)
      (if (or (= m 4)(= m 6)(= m 9)(= m 11))
          30
          31)))

(define (ultd dia m a)
  (if (= dia (dias m a))
      (set! dia 1)
      (set! dia (+ dia 1)))
  dia)

(define (ultm ))
(define (reloj)
  (define a 0)
  (define m 0)
  (set! m (read))
  (set! a (read))
  (dias m a)
  )