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
;FUNCIONES COMPLEMENTARIAS PARA LA FECHA DE VENCIMIENTO
(define (bisiesto a)
  (if (and (= 0 (remainder a 4)) (or (not(= 0 (remainder a 100))) (= 0 (remainder a 400))))
      #t
      #f))
(define (dias m a)
  (if (= m 2)
      (if (bisiesto a)
          29
          28)
      (if (or (= m 4)(= m 6)(= m 9)(= m 11))
          30
          31)))
(define (fecha? passw)
  (define d 0)
  (define m 0)
  (define a 0)
  (define fecha 0)
  (display "Digite el año de vencimiento (número): ")
  (set! a (read))
  (display "Digite el mes de vencimiento (número): ")
  (set! m (read))
  (display "Digite el día de vencimiento: ")
  (set! d (read))
  (if (< d (dias m a))
      (begin
        (set! fecha (vector (+ passw d) (+ m passw) (+ a passw)))
        fecha)
      (begin
        (display "Digite una fecha válida.\nEjecutando de nuevo.\n")
      (fecha? passw))))
;FUNCIÓN PARA MODIFICAR EL PRECIO, VALOR UNITARIO Y TOTAL
(define (valor passw basedatos)
  (define vu 0)
  (define vt 0)
  (define cu 0)
  (display "Digite la cantidad de unidades del producto: ")
  (set! cu (read))
  (vector-set! basedatos 5 (+ passw cu))
  (display "Digite el valor unitario del producto:")
  (set! vu (read))
  (vector-set! basedatos 6 (+ passw vu))
  (vector-set! basedatos 7 (+ passw (* vu cu)))
  basedatos
  )

;CREACIÓN DE FUNCIONES PARA MANIPULACIÓN DE LA BASE DE DATOS.
;-------------------------------
;FUNCIÓN PARA INGRESAR UN PRODUCTO NUEVO
(define (ingresar basedatos nombre passw ind)
  (define producto 0)
  (set! producto (llenar nombre passw ind))
  (vector-set! basedatos ind producto)
  (display basedatos)
  (menu basedatos nombre passw (+ ind 1)))

;FUNCIÓN PARA LLENAR CADA VECTOR CON DATOS DE UN PRODUCTO.
(define (llenar nombre passw ind)
  (define datos (make-vector 8 0))
  (define medida 0)
  (define nombre 0)
  (display "Por motivos de seguridad, debe ingresar la contraseña de encriptación del sistema..\nContraseña: ")
  (if (= (read) passw)
      (begin                  
        (printf "¿Cúal será la ID del producto que desea ingresar a la base de datos?(número)\n")
        (vector-set! datos 0 (+ (read) passw))
        (read-char);limpia el buffer
        (printf "¿Tipo de producto?\n")
        (vector-set! datos 1 (encarac (read-char) passw))
        (read-char)
        (display "¿Cúal es el nombre del producto?\n")
        (set! nombre (read-line))
        (vector-set! datos 2 (encriptar nombre passw 0 (make-string (string-length nombre) #\a)))
        (vector-set! datos 3 (fecha? passw))
        (display "Digite la unidad de medida del producto: ")
        (read-char)
        (set! medida (read-line))
        (vector-set! datos 4 (encriptar medida passw 0 (make-string (string-length medida) #\a)))
        (valor passw datos)
        )
      (begin
       (display "La contraseña no es correcta.\nEjecutando de nuevo.")
       (llenar datos nombre passw ind 1)
       ))
  datos
)

;FUNCIÓN PARA CREAR EL SUPERMERCADO
(define (crearsuper)
  (define basedatos 0)
  (define nombre 0)
  (define passw 0)
  (display "¿Cúal es el nombre de su supermercado?\n")
  (set! nombre (read-line))
  (printf "¿Cuántos productos tendrá ~a?\n" nombre)
  (set! basedatos (make-vector (read) 0))
  (display "\n¿Cúal es la contraseña de seguridad para la encriptación?\n")
  (set! passw (read))
  (menu basedatos nombre passw 0))
;FUNCIÓN PARA MOSTRAR EL MENÚ
(define (menu basedatos nombre passw ind)
  (display "\n     @@@@@                                         
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
  (usarmenu (read) basedatos nombre passw ind)
  )
;FUNCIÓN PARA LLAMAR A LAS FUNCIONES CORRESPONDIENTES.
(define (usarmenu op basedatos nombre passw ind)
  (cond
    [(= op 1)(begin
               (ingresar basedatos nombre passw ind)
               ;(menu basedatos nombre passw)
               )]
    [(= op 2)(begin
               
               (menu basedatos nombre passw))]
    [(= op 3)(begin
               
               (menu basedatos nombre passw))]
    [(= op 4)(begin
               
               (menu basedatos nombre passw))]
    [(= op 5)(begin
               
               (menu basedatos nombre passw))]
    [(= op 6)("¡MUCHAS GRACIAS POR USAR NUESTRA BASE DE DATOS!")]
    ))

(crearsuper)

















