#lang racket
(define (Minus? c)
  (cond
    [(and (> (char->integer c) 64)(< (char->integer c) 91))#f]
    [(and (> (char->integer c) 96)(< (char->integer c) 123))#t]))

(define (carac c sem)
  (if (Minus? c)
      (+ (remainder (+ sem (char->integer c)) 122) (* (quotient (+ (char->integer c) sem) 97)))
      (+ 65 (remainder (+ sem (char->integer c)) (char->integer c)))
      ))

(carac #\z 3) 
