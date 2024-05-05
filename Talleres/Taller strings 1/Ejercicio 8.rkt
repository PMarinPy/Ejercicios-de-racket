#lang racket
(define (agregar1 str strf ind)
  (if (< ind (string-length str))
      (begin
        (string-set! strf  ind (string-ref str ind))
        (agregar1 str strf (+ ind 1)))
      strf))

(define (agregar2 str strf ind i)
  (if (< i (string-length str))
      (begin
        (string-set! strf  ind (string-ref str i))
        (agregar2 str strf (+ ind 1) (+ i 1)))
      strf))

(define (append str1 str2)
  (define strf (make-string (+ (string-length str1)  (string-length str2)) #\a ))
  (agregar1 str1 strf 0)
  (display strf)
  (agregar2 str2 strf (string-length str1) 0))



(define (usandoapp str1 str2)
  (string-append str1 str2))

(append "holi" " que mas")