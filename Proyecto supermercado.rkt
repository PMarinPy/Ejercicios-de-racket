#lang racket
;ELABORADO POR JUAN PABLO MARÍN MARÍN.
;CÓDIGO PARA ENCRIPTAR Y DESENCRIPTAR STRINGS.
(define (letra? c)
  (if (or(and (> (char->integer c) 64)(< (char->integer c) 91)) (and (> (char->integer c) 96)(< (char->integer c) 122)))
      #t
      #f))
(define (Minus? c)
  (cond
    [(and (> (char->integer c) 64)(< (char->integer c) 91))#f]
    [(and (> (char->integer c) 96)(< (char->integer c) 122))#t]))
;FUNCIÓN QUE ENCRIPTA UN CARACTER
(define (encarac char sem)
  (define ind (char->integer char))
  (if (letra? char)
      (if (Minus? char)
          (integer->char (+ (* 96 (quotient (+ ind sem) 122)) (remainder (+ ind sem) 122)))
          (integer->char (+ (* 64 (quotient (+ ind sem) 91)) (remainder (+ ind sem) 91))))
      char))
;FUNCION QUE DESENCRIPTA UN CARACTER.
(define (desencarac char sem)
  (define ind (char->integer char))
  (if (letra? char)
      (if (Minus? char)
          (if (< (- ind sem) 96)
              (integer->char (- 122 (- 96 (- ind sem))))
              (integer->char (- ind sem)))
          (if (< (- ind sem) 64)
              (integer->char (- 91 (- 64 (- ind sem))))
              (integer->char (- ind sem))
              ))
      char))
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
  (if (and(integer? m)(integer? a)(integer? d)(<= d (dias m a))(< m 13))
      (begin
        (set! fecha (vector (+ passw d) (+ m passw) (+ a passw)))
        fecha)
      (begin
        (display "Digite una fecha válida.\nEjecutando de nuevo.\n")
        (fecha? passw))))
;FUNCIÓN PARA MODIFICAR EL PRECIO, VALOR UNITARIO Y TOTAL
(define (valoruni passw basedatos)
  (define vu 0)
  (define vt 0)
  (define cu 0)
  (display "Digite la cantidad de unidades del producto: ")
  (set! cu (read))
  (if (integer? cu)
      (begin
        (vector-set! basedatos 5 (+ passw cu))
        (display "Digite el valor unitario del producto:")
        (set! vu (read))
        (if (integer? vu)
            (begin
              (vector-set! basedatos 6 (+ passw vu))
              (vector-set! basedatos 7 (+ passw (* vu cu)))
              basedatos)
            (begin
              (display "Error en la digitaicón del valor unitario.\n")
              (valoruni passw basedatos)))
       )
      (begin
        (display "Error en la digitación de la cantidad de unidades el producto.\n")
        (valoruni passw basedatos))))
;FUNCIÓN PARA PEDIR ID Y NOMBRE
(define (idtipo datos passw)
  (define tipo 0)
  (define id 0)
  (printf "¿Cúal será la ID del producto que desea ingresar a la base de datos?(número)\n")
  (set! id (read))
  (if (integer? id)
      (begin
        (vector-set! datos 0 (+ id passw))
        (printf "¿Tipo de producto?\nL para lácteo.\nC Para cárnico.\nV para vegetal.\nG para grano.\nE para enlatado.\nA para aseo.\n")
        (read-char)
        (set! tipo (read-line))
        (if (and(string? tipo)(esprod? (string-ref tipo 0)))
            (begin
              (vector-set! datos 1 (encarac (string-ref tipo 0) passw))
              datos)
            (begin
              (display "Error en la digitación del tipo de producto. (solo la inical).\n")
              (idtipo datos passw))))
      (begin
        (display "La ID debe ser un número entero.\n")
        (idtipo datos passw))
      ))
;FUNCIÓN PARA EL NOMBRE Y LA UNIDAD DE MEDIDA
(define (nombreuni datos passw)
  (define medida 0)
  (define nombre 0)
  (display "¿Cúal es el nombre del producto?\n")
  (set! nombre (read-line))
  (if (string? nombre)
      (begin
        (vector-set! datos 2 (encriptar nombre passw 0 (make-string (string-length nombre) #\a)))
        (display "Digite la unidad de medida del producto: ")
        (set! medida (read-line))
        (if (string? medida)
            (begin
              (vector-set! datos 4 (encriptar medida passw 0 (make-string (string-length medida) #\a)))
              datos)
            (begin
              (display "Error en la digitación de la medida.\n")
              (nombreuni datos passw))))
      (begin
        (display "Error en la digitación del nombre del producto.\n")
        (nombreuni datos passw))
      ))
;FUNCIÓN PARA BUSCAR UNA ID
(define (buscarid basedatos passw id ind)
  (define producto 0)
  (if (and (not (integer? (vector-ref basedatos ind)))(< ind (vector-length basedatos)))
      (begin
        (set! producto (vector-ref basedatos ind))
        (if (= (- (vector-ref producto 0) passw) id)
          ind
          (buscarid basedatos passw id (+ ind 1))))
      -1))
;
;FUNCIÓN PARA VALIDAR SI ES UN TIPO DE PRODUCTO VÁLIDO.
(define (esprod? prod)
  (if (or(char-ci=? prod #\L )(char-ci=? prod #\C )(char-ci=? prod #\G )(char-ci=? prod #\V )(char-ci=? prod #\A )(char-ci=? prod #\E ))
      #t
      #f))
;FUNCIÓN PARA MOSTRAR LA FECHA DE VENCIMIENTO
(define (mostrarfecha fecha passw)
  (display "\nDIA DE VENCIMIENTO: ")
  (displayln (- (vector-ref fecha 0) passw))
  (display "MES DE VENCIMIENTO: ")
  (displayln (- (vector-ref fecha 1) passw))
  (display "AÑO DE VENCIMIENTO: ")
  (displayln (- (vector-ref fecha 2) passw)))
;FUNCIÓN PARA MOSTRAR LOS PRODUCTOS DEL TIPO QUE INDIQUE EL USUARIO.
(define (mostrarprods tipo basedatos ind passw)
  (define producto 0)
  (define nombre 0)
  (define unidad 0)
  (if (and (< ind (vector-length basedatos))(not(integer? (vector-ref basedatos ind))))
      (begin
        (set! producto (vector-ref basedatos ind))
        (if (char-ci=? tipo (desencarac (vector-ref producto 1) passw))
            (begin
              (display "ID DEL PRODUCTO: ")
              (displayln (- (vector-ref producto 0) passw))
              (display "---------------------------------\n")
              (display "TIPO DE PRODUCTO: \n")
              (cond
                [(char-ci=? tipo #\a )(displayln "Aseo")]
                [(char-ci=? tipo #\c )(displayln "Cárnico")]
                [(char-ci=? tipo #\l )(displayln "Lácteo")]
                [(char-ci=? tipo #\v )(displayln "Vegetal")]
                [(char-ci=? tipo #\e )(displayln "Enlatado")]
                [(char-ci=? tipo #\g )(displayln "Grano")]
                )
              (display "---------------------------------\n")                        
              (set! nombre (vector-ref producto 2))
              (display "NOMBRE DEL PRODUCTO: ")
              (display (desencrip nombre passw 0 (make-string (string-length nombre) #\a)))
              (mostrarfecha (vector-ref producto 3) passw)
              (display "---------------------------------\n")  
              (display "UNIDAD DEL PRODUCTO: ")
              (set! unidad (vector-ref producto 4))
              (display (desencrip unidad passw 0 (make-string (string-length unidad) #\a )))
              (display "\n---------------------------------\n")  
              (display "CANTIDAD DE UNIDADES: ")
              (displayln (- (vector-ref producto 5) passw))
              (mostrarprods tipo basedatos (+ ind 1) passw)
            )
            (begin
              (mostrarprods tipo basedatos (+ ind 1) passw))
            ))
      (begin
        (display ""))))
;FUNCÍON DE TIPO DE PRODUCTO.
(define (mostrartipo nombre basedatos passw ind)
  (define tipo 0)
  (display "Por motivos de seguridad, debe ingresar la contraseña de encriptación del sistema..\nContraseña: ")
  (if (= (read) passw)
      (begin
        (display "¿Qué tipo de producto desea ver?\nL para lácteo.\nC Para cárnico.\nV para vegetal.\nG para grano.\nE para enlatado.\nA para aseo.\n")
        (read-char)
        (set! tipo (read-line))
        (set! tipo (string-ref tipo 0))
        (if (esprod? tipo)
            (begin
              (mostrarprods tipo basedatos 0 passw)
              (menu basedatos nombre passw (+ ind 1))
              )
            (begin
              (display "No es un producto válido.")
              (mostrartipo nombre basedatos passw ind))
            ))        
      (begin
       (display "La contraseña no es correcta.\nEjecutando de nuevo.")
       (mostrartipo nombre basedatos passw ind)
       )))
;FUNCIÓN PARA MOSTRAR PRODUCTOS DE UN PRECIO DADO.
(define (llamarproduc basedatos nombre passw ind)
  (define precio 0)
  (display "Digite la contraseña de encriptación: ")
  (if (= (read) passw)
     (begin
       (display "Digite el precio base para ver productos de precio mayor.\nPrecio: ")
       (set! precio (read))
       (mostrarprecio basedatos precio passw 0)
       (menu basedatos nombre passw ind))
     (begin
       (display "\nContraeña inválida..\n")
       (menu basedatos nombre passw ind)
       )))
  ;MUESTRA A PARTIR DE PRODUCTOS.
(define (mostrarprecio basedatos precio passw ind)
  (define producto 0)
  (define nombre 0)
  (define unidad 0)  
      (if (and(< ind (vector-length basedatos))(not(integer? (vector-ref basedatos ind))))
          (begin
            (set! producto (vector-ref basedatos ind))      
            (if (> (- (vector-ref producto 6) passw) precio)
                (begin        
                  (set! nombre (vector-ref producto 2))
                  (display "NOMBRE DEL PRODUCTO: ")
                  (display (desencrip nombre passw 0 (make-string (string-length nombre) #\a)))
                  (display "\n---------------------------------\n")
                  (display "PRECIO UNITARTIO DEL PRODUCTO: ")
                  (display (- (vector-ref producto 5) passw))
                  (display "\n---------------------------------\n")
                  (displayln "CANTIDAD DE UNIDADES: ")
                  (displayln (- (vector-ref producto 6) passw))
                  (display "---------------------------------\n")
                  (display "VALOR TOTAL DE LOS PRODUCTOS: ")
                  (displayln (- (vector-ref producto 7) passw))
                  (mostrarprecio basedatos precio passw (+ 1 ind)))
                (mostrarprecio basedatos precio passw (+ 1 ind))))
          (display "")))
;MUESTRA UNIDADES
(define (unidades basedatos unidad passw ind)
  (define producto 0)
  (define nombre 0)
  (define tipo 0)
  (if (and(< ind (vector-length basedatos))(not(integer? (vector-ref basedatos ind))))
      (begin
        (set! producto (vector-ref basedatos ind))
        (if (string-ci=? unidad (desencrip (vector-ref producto 4) passw 0 (make-string (string-length (vector-ref producto 4)) #\a)))
            (begin
              (set! tipo (desencarac (vector-ref producto 1) passw))
              (display "ID DEL PRODUCTO: ")
              (displayln (- (vector-ref producto 0) passw))
              (display "---------------------------------\n")
              (display "TIPO DE PRODUCTO: \n")
              (cond
                [(char-ci=? tipo #\a )(displayln "Aseo")]
                [(char-ci=? tipo #\c )(displayln "Cárnico")]
                [(char-ci=? tipo #\l )(displayln "Lácteo")]
                [(char-ci=? tipo #\v )(displayln "Vegetal")]
                [(char-ci=? tipo #\e )(displayln "Enlatado")]
                [(char-ci=? tipo #\g )(displayln "Grano")]
                )
              (display "---------------------------------\n")                        
              (set! nombre (vector-ref producto 2))
              (display "NOMBRE DEL PRODUCTO: ")
              (display (desencrip nombre passw 0 (make-string (string-length nombre) #\a)))
              (mostrarfecha (vector-ref producto 3) passw)
              (display "---------------------------------\n") 
              (display "VALOR UNITARIO DE UNIDADES: ")
              (displayln (- (vector-ref producto 6) passw))
              (unidades basedatos unidad passw (+ ind 1))
              )
            (unidades basedatos unidad passw (+ ind 1))))
      (display "")))
;FUNCIÓN PARA MOSTRAR PRODCUTOS A PARTIR DE TIPO DE UNIDAD
(define (mostraruni basedatos nombre passw ind)
  (define unidad 0)
  (display "Por motivos de seguridad debe ingresar la contraseña...\nContraseña: ")
  (if (= (read) passw)
      (begin
        (display "¿Qué tipo de productos desea ver?\nUnidad (Unidad, litros, gramos. kilogramos, etc): ")
        (read-char)
        (set! unidad (read-line))
        (unidades basedatos unidad passw 0)
        (menu basedatos nombre passw ind)               
        )
      (begin
        (display "Constraseña incorrecta...\n")
        (menu basedatos nombre passw ind))))
;CREACIÓN DE FUNCIONES PARA MANIPULACIÓN DE LA BASE DE DATOS.
;-------------------------------
;FUNCIÓN PARA INGRESAR UN PRODUCTO NUEVO
(define (ingresar basedatos nombre passw ind)
  (define producto 0)
  (define contra 0)
  (display "Por motivos de seguridad, debe ingresar la contraseña de encriptación del sistema..\nContraseña: ")
  (if (= (read) passw)
      (begin
        (set! producto (llenar nombre passw ind))
        (vector-set! basedatos ind producto)
        (display basedatos)
        (menu basedatos nombre passw (+ ind 1)))
      (begin
       (display "La contraseña no es correcta.\nEjecutando de nuevo.")
       (ingresar basedatos nombre passw ind)
       )))
;FUNCIÓN PARA LLENAR CADA VECTOR CON DATOS DE UN PRODUCTO.
(define (llenar nombre passw ind)
  (define datos (make-vector 8 0))  
  (idtipo datos passw)
  (nombreuni datos passw)
  (vector-set! datos 3 (fecha? passw))
  (valoruni passw datos)
  datos)
;FUNCIÓN PARA MODIFICAR DATOS
(define (modificar basedatos nombre passw ind)
  (display "Por motivos de seguridad, debe ingresar la contraseña de encriptación del sistema..\nContraseña: ")
  (if (= (read) passw)
      (begin
        (cambiardatos basedatos nombre passw ind)
        (display basedatos)
        (menu basedatos nombre passw ind))
      (begin
        (display "La contraseña no es correcta.\nEjecutando de nuevo.")
        (modificar basedatos nombre passw ind)
        )))
;FUNCIÓN PARA MODIFICAR LA FECHA DE VENCIMIENTO O VALOR UNITARIO.
(define (cambiardatos basedatos nombre passw ind)
  (define datos 0)
  (define id 0)
  (define op 0)
  (define ind 0) 
  (display "¿Cúal es la ID del producto que desea modificar?\n")
  (set! id (read))
  (- id passw)
  (set! ind (buscarid basedatos passw id 0))
  (if (not(= ind -1))
      (begin
        (set! datos (vector-ref basedatos ind))
        (display "¿Qué desea cambiar de su producto?\nDigite 1 para modificar la fecha de vencimiento.")
        (display "\nDigite 2 para modificar el valor unitario.\nDigite 3 para modificar la fecha y el valor unitario")
        (set! op (read))
        (cond
          [(= op 1)(vector-set! datos 3 (fecha? passw))]
          [(= op 2)(valoruni passw datos)]
          [(= op 3) (begin
                      (vector-set! datos 3 (fecha? passw))
                      (valoruni passw datos))]))
      (begin
        (display "Producto no encontrado.\n")
        (menu basedatos nombre passw ind)
        ))
  datos)

;FUNCIÓN PARA CREAR EL SUPERMERCADO
(define (crearsuper)
  (define basedatos 0)
  (define nombre 0)
  (define passw 0)
  (display "¿Cúal es el nombre de su supermercado?\n")
  (set! nombre (read-line))
  (printf "¿Cuántos productos tendrá ~a?\n" nombre)
  (set! basedatos (make-vector (read) 0))
  (display "¿Cúal es la contraseña de seguridad para la encriptación?\n")
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
               )]
    [(= op 2)(begin               
               (modificar basedatos nombre passw ind))]
    [(= op 3)(begin               
               (mostrartipo nombre basedatos passw ind))]
    [(= op 4)(begin               
               (llamarproduc basedatos nombre passw ind))]
    [(= op 5)(begin               
               (mostraruni basedatos nombre passw ind))]
    [(= op 6)"¡MUCHAS GRACIAS POR USAR NUESTRA BASE DE DATOS!"]
    ))

(crearsuper)

















