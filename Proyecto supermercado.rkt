#lang racket
;ELABORADO POR JUAN PABLO MARÍN MARÍN.
;CÓDIGO PARA ENCRIPTAR Y DESENCRIPTAR STRINGS.
(define (Minus? c)
  (cond
    [(and (> (char->integer c) 64)(< (char->integer c) 91))#f]
    [(and (> (char->integer c) 96)(< (char->integer c) 123))#t]))
;FUNCIÓN QUE ENCRIPTA UN CARACTER
(define (encarac char sem)
  (define ind (char->integer char))
  (if (Minus? char)
      (integer->char (+ (* 97 (quotient (+ ind sem) 123)) (remainder (+ ind sem) 123)))
      (integer->char (+ (* 65 (quotient (+ ind sem) 92)) (remainder (+ ind sem) 92)))))
;FUNCION QUE DESENCRIPTA UN CARACTER.
(define (desencarac char sem)
  (define ind (char->integer char))
  (if (Minus? char)
      (if (< (- ind sem) 97)
          (integer->char (- 123 (- 97 (- ind sem))))
          (integer->char (- ind sem)))
      (if (< (- ind sem) 65)
          (integer->char (- 91 (- 65 (- ind sem))))
          (integer->char (- ind sem))
          )))
;FUNCIÓN QUE ENCRIPTA STRINGS
(define (encriptar str sem ind strf)
  (if (< ind (string-length str))
      (begin
        (string-set! strf ind (encarac (string-ref str ind) sem))
        (encriptar str sem (+ ind 1) strf))
      strf))
;FUNCION PARA DESENCRIPTAR
(define (desencrip str sem ind strf)
  (if (< ind (string-length str))
      (begin
        (string-set! strf ind (desencarac (string-ref str ind) sem))
        (desencrip str sem (+ ind 1) strf))
      strf))
;