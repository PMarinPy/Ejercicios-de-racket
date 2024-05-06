#lang racket
;EJERCICIO 1
(define (eschar c)
  (if (char? c)
      #t
      #f)
  )
;EJERCICIO 2
(define (EsVocal a)
  (if (or (= (char->integer a) 97) (= (char->integer a) 101) (= (char->integer a) 65) (= (char->integer a) 69) (= (char->integer a) 85) (= (char->integer a) 73) (= (char->integer a)79)
             (= (char->integer a) 105)(= (char->integer a) 111)(= (char->integer a) 105))
      #t
      #f
      ))
-;EJERCICIO 3
(EsVocal #\U)
(define (vocal c)
  (if (EsVocal c)
      (display "Es una vocal")
      (display "No es una vocal ")))
;EJERCICIO 4
(define (comp c1 c2)
  (cond
    [(char=? c1 c2)(displayln "Los caracteres son iguales")]
    [(< (char->integer c1) (char->integer c2))(printf "El caracter ~a es mayor que ~a en la tabla ASCII\n" c2 c1)]
    [(> (char->integer c1) (char->integer c2))(printf "El caracter ~a es mayor que ~a en la tabla ASCII\n" c1 c2)]))

(define (ppal)
  (define c1 0)
  (define c2 0)
  (display "Función para comparar dos caracteres y calcular si uno es mayor a otro.\nDigite el primer caracter: ")
  (set! c1 (read-char))
  (read-char)
  (display "Digite el segundo caracter: ")
  (set! c2 (read-char))
  (read-char)
  (comp c1 c2))
;EJERCICIO 5
(define (letra? c)
  (cond
    [(= (char->integer c) 32)(displayln "El caracter es un espacio en blanco.\n")]
    [(and (> (char->integer c) 64)(< (char->integer c) 91))(displayln "Es una letra Mayúscula.\n")]
    [(and (> (char->integer c) 96)(< (char->integer c) 123))(displayln "Es una letra minúscula.\n")]
    (else (displayln "No es alfabético ni es un espacio en blanco.\n"))))
(define (esletra)
  (define c 0)
  (display "Función para retornar si un carcter es aflabético o un espacio.\nDigite el caracter: ")
  (set! c (read-char))
  (letra? c)
  (esletra))

;EJERICIO 6
(define (conv n)
  (cond
    [(integer? n)(printf "El caracter correspondiente al número ~a en la tabla ASCII es: ~a.\n\n" n (integer->char n))]
    [(char? n)(printf "El índice del caracter ~a en la tabla ASCII es: ~a\n\n" n (char->integer n))]))
(define (convertir)
  (define n 0)
  (define o 0)
  (display "Función para mostrar un caracter o su índice de la tabla ASCII.\nDigite 1 para acceder al caracter o 2 para retornar el índice.\n")
  (set! o (read))

  (if (> o 2)
      (display "¡ERROR!")
      (if (= o 1)
          (begin
            (display "Digite el número: ")
            (conv (read)))
          (begin
            (display "Digite el caracter: ")
            (read-char)
            (conv (read-char))))))

;EJERICICIO 7
(define (alfa? c)
  (cond
    [(and (> (char->integer c) 64)(< (char->integer c) 91))(displayln "Es una letra Mayúscula.\n")]
    [(and (> (char->integer c) 96)(< (char->integer c) 123))(displayln "Es una letra minúscula.\n")]))
(define (alfabetico)
  (display "Función para ver si un caracter es mayuscula o minuscula.\nDigite la variable: ")
  (alfa? (read-char)))

;EJERCICIO 8
(define (pasar)
  (define c 0)
  (display "Función para convertir una letra de mayuscula a minuscula y viceversa.\nDigite el caracter: ")
  (set! c (read-char))
  (if (char? c)
      (if (and(> (char->integer c) 64)(< (char->integer c) 91))
          (display (integer->char (+ 32 (char->integer c))))
          (if (and (> (char->integer c) 96)(< (char->integer c) 123))
              (display (integer->char (- (char->integer c) 32)))
              (display "No es una letra del afabeto.\n")))
      (display "No es un caracter")))
;EJERCICIAO 9
(define (recursivo S N n num e)
    (define a 0)
  (display "Ejecutando función,figite N para seguir ejecutando y S para parar ")
  (set! a (read-char))
  (cond
    [(char=? a #\S)(begin
                (printf"Digitó la letra N ~a veces, digitó un error ~a veces;" N e)
                (printf "~a fueron números, ~a fueron n" num n))]
    [(number? a)(begin
                  (read-char)
                  (recursivo S N n (+ num 1) (+ e 1)))]
    [(char=? #\n a)(begin
                     (read-char)
                     (recursivo S N (+ n 1) num (+ e 1)))]
    [(char=? #\N a)(begin
                     (read-char)
                     (recursivo S (+ N 1) n num e))]
    (else (begin
            (read-char)
            (recursivo S N n num (+ e 1))))))
;(recursivo 0 0 0 0 0)
(define (cifrar c d)
  (+ 97 (remainder (+ d c) c)))
(define (cipher)
  (define n 0)
  (define d 0)
  (display "¿Cual es el desplazamiento del cifrado?(solo minusculas) ")
  (set! d (read))
  (read-char)
  (display "Digite la letra: ")
  (set! n (read-char))
  (define f (char->integer n))
  (display "La letra minúscula cifrada es: ")
  (integer->char (cifrar f d)))
(cipher)






  
