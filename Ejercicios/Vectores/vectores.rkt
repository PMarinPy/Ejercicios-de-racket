#lang racket
;Vectores
;la sintaxis es:
(make-vector 20 0.0)
;crea un vector con 20 datos, cada uno con 0.0


;vector-ref accede a un elemento de el vector:
;(vector-ref vectoe_a_ver valor_a_ver)
(define nota1 (make-vector 20 0.0))
(vector-ref nota1 4)


;setear un valor de un vector
(vector-set! nota1 p (+ 1 2))

;(vector-set! vector_a_modificar posicion valor)
