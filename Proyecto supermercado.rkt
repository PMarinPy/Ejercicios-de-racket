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

;CREACIÓN DE FUNCIONES PARA MANIPULACIÓN DE LA BASE DE DATOS.
;-------------------------------
;FUNCIÓN PARA 

;FUNCIÓN PARA CREAR EL SUPERMERCADO
(define (crearsuper)
  (define basedatos 0)
  (define nombre 0)
  (display "¿Cúal es el nombre de su supermercado?\n")
  (set! nombre (read-line))
  (printf "¿Cuántos productos tendrá ~a?\n" nombre)
  (set! basedatos (make-vector (read) (make-vector 8 0)))
  (menu basedatos nombre))
;FUNCIÓN PARA MOSTRAR EL MENÚ
(define (menu basedatos nombre)
  (display "     @@@@@                                         
         .@                                         
          @@                                        
          @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@  
           @@        @@       @@        @@       @@ 
           @@        @@       @@        @@      @@  
            @@       @@       @@       @@       @@  
            @@       @@       @@       @@       @@  
             @       @@       @@       @@      @@   
             @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@   
             @@#      @@      @@      %@%     @@:   
              @@      @@      @@      @@      @@    
              @@      @@      @@      @@      @@    
               @@     :@:     @@      @@     #@     
               @@      @@     @@      @@     @@     
             @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@      
           @@                                       
           @@                                       
            @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@      
                 @@@    @@          @@@   .@@       
                 @@     @@          @@     @@       
                  @@   .@#           @@   -@*       
                    @@@%               @@@#   \n")
  (printf "¡BIENVENIDO AL SUPERMERCADO ~a!\n" nombre)
  (display "¿Qué desea hacer?\n")
  (displayln "+------------------------------------------------+")
  (displayln "|                   Menú Principal               |")
  (displayln "+------------------------------------------------+")
  (displayln "| Opción |            Descripción                |")
  (displayln "+------------------------------------------------+")
  (displayln "|   1    |  Ingreso de un producto nuevo         |")
  (displayln "|   2    |  Actualización de datos de un producto|")
  (displayln "|   3    |  Listado de productos por tipo        |")
  (displayln "|   4    |  Listado de productos por valor       |")
  (displayln "|   5    |  Listado general de productos         |")
  (displayln "|   6    |  Salir                                |")
  (displayln "+------------------------------------------------+")
  )
;FUNCIÓN PARA LLAMAR A LAS FUNCIONES CORRESPONDIENTES.
(define (usarmenu op basedatos nombre)
  (cond
    [(= op 1)(begin
               
               (menu basedatos nombre))]
    [(= op 2)(begin
               
               (menu basedatos nombre))]
    [(= op 3)(begin
               
               (menu basedatos nombre))]
    [(= op 4)(begin
               
               (menu basedatos nombre))]
    [(= op 5)(begin
               
               (menu basedatos nombre))]
    [(= op 6)("¡MUCHAS GRACIAS POR USAR NUESTRA BASE DE DATOS!")]
    ))

(crearsuper)

















