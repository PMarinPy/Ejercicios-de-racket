#lang racket
;# VERSIÓN CON EL DELETE siu
(define (puntopop vec num ind)
  (if (> ind 0)
      (if (= (vector-ref vec ind) num)
          (begin
            (vector-set!)))))