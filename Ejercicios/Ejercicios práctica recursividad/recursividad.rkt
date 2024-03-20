#lang racket
;numeros del x al y
(define (mayoramenor x y)
  (if (>= x y)
      (begin
        (displayln x)
        (mayoramenor (- x 1) y))
      (display "")))
;Mostrar número de x a y, siendo x menor
(define (num x y)
  (if (<= x y)
      (begin
        (displayln x)
        (num (+ 1 x) y))
      (display "")))
;cuatro operaciones
(define (ops x y)
  (if (<= x y)
      (begin
        (display x)
        (display ": El cuadrado es: ")
        (display (expt x 2))
        (display "  --- el cubo es: ")
        (display (expt x 3))
        (display "  --- la raiz es: ")
        (display (expt x (/ 1 2)))
        (display "  --- raiz cúbica es: ")
        (displayln (expt x (/ 1 3)))
        (newline)
        (ops (+ 1 x) y))
      (display "")))
;sumar números
(define (sumar x y s)
  (if (<= x y)
      (sumar (+ x 1) y (+ x s))
      (display "")))
;contar números
(define (contar x y c)
  (if (<= x y)
      (begin
        (set! c (+ c 1))
        (contar (+ 1 x) y c))
      (c)))
;cuadrados de un número a otro
(define (cuentacua x y)
  (if (< x y)
      (begin
        (display (expt x 2))
        (cuentacua (+ x 1) y))
      (display "")))
;suma de numeros menores
(define (menores x y s)
  (if (> x y)
      (menores (- x 1) y (+ s (- x 1)))
      (display s)))
;cuadrados de un número a otro sin incluirlos 
(define (cuadrados x y s)
  (if (> (- y 1) x)
      (begin
        (cuadrados (+ x 1) y (+ s (expt (+ x 1) 2))))
      s))
;Hallar promedio
(define (promedio x y c d)
  (if (<= x y)
      (begin
        (promedio (+ x 1) y (+ c 1) (+ d x)))
      (display (/ d c))))
;cuadrado y cubo de cada número
(define (cuacu x)
  (define n 0)
  (if (< x 10)
      (begin
        (display "Número: ")
        (set! n (read))
        (printf "\nNúnmero al cuadrado: ~a / Número al cubo: ~a \n\n" (expt n 2)(expt n 3))
        (cuacu (+ x 1)))
      (display "")))
;mostrar ciertos números dependiendo de otro
(define (numos x)
  
  (cond
    ((<= x 5)(num 1 5)) ;usé la función que tenía para mostrar
    ((and (> x 5)(<= x 10))(num 5 10))
    (else (num 10 20))))

;funcionsuprema
(define (funcionsuprema)
  (define x 0)
  (define y 0)
  (define c 0)
  (define d 0)
  (define o 0)
  (display "\nEscriba el número correspondiente para usar la función que desee\n
1. Numeros del mayor al menor
2. Mostrar número del menor al mayor
3. Cuatro operaciones.(raices y cubos)
4. Sumar números incluyéndolos
5. Contar números
6. Mostrar cuadrados de un número a otro
7. Suma de numeros menores al segundo
8. Sumar cuadrados de un número a otro sin incluirlos 
9. Hallar promedio
10. Cuadrado y cubo de cada número (10 números)
11. Mostrar ciertos números dependiendo de otro
12. Salir\n ")
  (set! o (read))
  (display "\nEscriba el primer número: ")
  (set! x (read))
  (display "Escriba el segundo número (si no es necesario introduzca 0): ")
  (set! y (read))
  (if (and (integer? o)(integer? x)(integer? y))
      (cond
        [(= o 1)(mayoramenor x y)]
        [(= o 2)(num x y)]
        [(= o 3)(ops x y)]
        [(= o 4)(sumar x y c)]
        [(= o 5)(contar x y c)]
        [(= o 6)(cuentacua x y)]
        [(= o 7)(menores x y c)]
        [(= o 8)(cuadrados x y c)]
        [(= o 9)(promedio x y c d)]
        [(= o 10)(cuacu c)]
        [(= o 11)(numos x)]

        )
      
      (display "Opción no válida.\nEjecutando de nuevo."))
  (newline)
  (funcionsuprema))
(funcionsuprema)
