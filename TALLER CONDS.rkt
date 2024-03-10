#lang racket
;EJERCICIO 1
(define (solucionar_falla_pc n)
  (cond
    ((= n 1) (displayln "1. El computador no enciende. Revise Conexión"))
    ((= n 2) (displayln "2. El computador se bloquea después de 10 minutos. Vacunar equipo"))
    ((= n 3) (displayln "3. El computador se bloquea cuando abro varias aplicaciones. Aumentar capacidad de memoria"))
    (else (displayln "Número de error no válido"))))

;EJERCICIO 2
(define (solucionar_nota numero)
  (cond
    ((>= numero 5)
     (begin
       (displayln "Excelente")
       (display (* numero 50))))
    ((>= numero 3)
     (begin
       (displayln "Bueno")
       (display (* numero 20))))
    (else
     (begin
       (displayln "Malo")
       (display (* numero 10))))))

;EJERCICIO 3
(define (reproducir_libro n)
  (cond
    ((= n 1) (displayln "reproducir EL CAPITAL"))
    ((= n 2) (displayln "reproducir EL CÓDIGO DA VINCI"))
    ((= n 3) (displayln "reproducir HARRY POTTER AND THE HALF BLOOD PRINCE"))
    ((= n 4) (displayln "reproducir CIEN AÑOS DE SOLEDAD"))
    ((= n 5) (displayln "reproducir LA ODISEA"))
    (else (displayln "Libro no válido"))))

;EJERCICIO 4
(define (reproducir_musica n)
  (cond
    ((= n 1) (displayln "C:/Musica/RAP"))
    ((= n 2) (displayln "C:/Musica/HEAVY METAL"))
    ((= n 3) (displayln "C:/Musica/ROCK"))
    ((= n 4) (displayln "C:/Musica/REGGAETON"))
    ((= n 5) (displayln "C:/Musica/SALSA"))
    ((= n 6) (displayln "C:/Musica/VALLENATO"))
    (else (displayln "Opción de música no válida"))))

;EJERCICIO 5
(define (cuantas_soluciones a b c)
  (cond
    ((= a 0) (displayln "La ecuación es degenerada y no se consideran cuántas soluciones tiene"))
    ((< (expt b 2) (* 4 a c)) (displayln "La ecuación tiene dos soluciones"))
    ((= (expt b 2) (* 4 a c)) (displayln "La ecuación tiene una solución"))
    ((> (expt b 2) (* 4 a c)) (displayln "La ecuación no tiene soluciones"))))

;EJERCICIO 6
(define (que_hacer aleatorio)
  (cond
    ((= aleatorio 1) (displayln "JUEGA RESIDENT EVIL"))
    ((= aleatorio 2) (displayln "¡PONTE A DORMIR, YA!"))
    ((= aleatorio 3) (displayln "JUEGA WARCRAFT"))
    ((= aleatorio 4) (displayln "SOLO DEDÍCATE A BAILAR"))
    ((= aleatorio 5) (displayln "ESCUCHA MÚSICA"))
    ((= aleatorio 6) (displayln "VE AL CINE"))
    ((= aleatorio 7) (displayln "COMPRA 10 CERVEZAS Y ÉCHATE A VER FÚTBOL 5 HORAS"))
    (else (displayln "Opción no válida"))))

;EJERCICIO 7
(define (menu)
  (displayln "Selecciona una opción:")
  (displayln "1. Solucionar falla de PC")
  (displayln "2. Nota")
  (displayln "3. Reproducir libro")
  (displayln "4. Reproducir música")
  (displayln "5. Cuantas soluciones (ecuación cuadrática)")
  (displayln "6. Qué hacer")
  (displayln "7. Salir")
  (let ((opcion (read)))
    (cond
      ((= opcion 1)
       (displayln "Ingrese el número de error:")
       (solucionar_falla_pc (read))
       (menu))
      ((= opcion 2)
       (displayln "Ingrese la nota:")
       (solucionar_nota (read))
       (menu))
      ((= opcion 3)
       (displayln "Ingrese el número de libro:")
       (reproducir_libro (read))
       (menu))
      ((= opcion 4)
       (displayln "Ingrese el número de música:")
       (reproducir_musica (read))
       (menu))
      ((= opcion 5)
       (displayln "Ingrese los coeficientes a, b, y c:")
       (cuantas_soluciones (read) (read) (read))
       (menu))
      ((= opcion 6)
       (displayln "Qué hacer:")
       (que_hacer (random 7))
       (menu))
      ((= opcion 7) (displayln "Adiós"))
      (else (begin
              (displayln "Opción no válida, inténtalo de nuevo.")
              (menu))))))

(menu)
