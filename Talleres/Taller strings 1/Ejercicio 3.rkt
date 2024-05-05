#lang racket
(define (contar s esp cont)
  (if (< cont (string-length s))
      (contar s (if (char=? (string-ref s cont) #\ )
                    (+ esp 1)
                    esp) (+ cont 1) )
      (+ esp 1)))

(define (crearstr str)
  (define strf  (make-string (contar str 0 0) #\a))
  (string-set! strf 0 (indmin (string-ref str 0)))
  strf
  )
(define (indmin c)
  (integer->char (+ (char->integer c) 32)))

(define (minus str strf ind inds)
  (if (< ind (string-length str))

      (if (char=? (string-ref str ind) #\ )
          (begin
            (string-set! strf inds (indmin (string-ref str (+ ind 1))))
            (minus str strf (+ ind 1)(+ inds 1)))
          (minus str strf (+ ind 1) inds))
      strf))

(define (ppal)
  (define str 0)
  (define strf 0)
  (display "Digite el nombre: ")
  (set! str (read-line))
  (set! strf (minus str (crearstr str) 0 1))
  (printf "El dominio de su correo es: ~a@utp.edu.co\n" strf)
  (ppal))
(ppal)