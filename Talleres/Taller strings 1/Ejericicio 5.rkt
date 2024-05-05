#lang racket
(define (palin str ind )
  (if (< ind (floor (/ (string-length str) 2)))
      (if (char-ci=? (string-ref str ind) (string-ref str (- (string-length str) 1 ind)))
          (palin str (+ ind 1))
          #f)
      #t))
