#lang racket
(define (buscarplaca placa indice vecplacas espacio)
  (if (< indice espacio)
      (if (= (vector-ref vecplacas indice) placa)
          indice
          (buscarplaca placa (+ indice 1) vecplacas espacio))
      -1))

(define (desocupado vecplacas indice espacio)
  (if (< indice espacio)
      (if (= -1 (vector-ref vecplacas indice))
          indice
          (desocupado vecplacas (+ indice 1)))
      -1))

(define (ingreso vecplacas vechoras indice espacio espmod)
  (if (= espacio (- espacio espmod))            
      (display "No hay espacio en el parqueadero. ")
      (if (< indice espacio)
          (begin
            (display "Por favor digite la placa de el auto: ")
            (vector-set! vecplacas (desocupado vecplacas 0) (read))
            (display "Por favor digite la hora de ingreso: ")
            ((vector-set! vechoras )(desocupado vechoras 0) (read))
            (- espmod 1))
          (display "ERROR EN LA FUNCION DE INGRESO")
          )))


(define (salida vecplacas vechoras espacio espmod)
  (define indice 0)
  (define h 0)
  (display "Por favor digite la placa: ")
  (set! indice (buscarplaca (read) 0 vecplacas espacio))
  (display "Hora de salida: ")
  (set! h (read))
  (display "Total a pagar: ")
  (display (* 1800 (- h (vector-ref vechoras indice))))
  (displayln "Que pase un buen día")
  (vector-set! vecplacas indice -1)
  (vector-set! vechoras indice -1)
  (set! espmod (+ espmod 1)))

(define (horastotales vecplacas vechoras espmod espacio)
  (define placa 0)
  (define h 0)
  (display "Digite la placa del vehículo: ")
  (set! placa (read))
  (display "¿Qué hora es? ")
  (set! h (read))
  (display "Las horas que lleva el usuario en el parqueadero son: ")
  (display (- h (vector-ref vechoras (buscarplaca placa 0 vecplacas espacio))))
  )
;//FUNCION QUE DICE CUANTOS ESPACIOS HAY DISPONIBLES 
(define (disponibles espmod espacio)
  (printf "Los espacios disponibles son: ~a" (- espmod espacio)))

(define (operarocho vechoras indice hora cantidad espacio)
  (if (< indice espacio)
      (if (>= (vector-ref vechoras indice)8)
         (operarocho vechoras indice hora (+ 1 cantidad))
         (operarocho vechoras indice hora cantidad))
      cantidad))

(define (ochoras vechoras)
  (define h 0)
  (display "¿Qué hora es?")
  (set! h (read))
  (printf "Los vehículos que llevan 8 horas son: ")
  (operarocho vechoras 0 h 0))

(define (pedirn)
  (display "
((((///((((((((((((((((//((((((((((((((/(/((((((((((((((((/(((((((((((((((/((((((((((((((((//(((((((
(((//((((((((((((((((((((((((((((((((/(((((((((((((((((/(((((((((((((((//(((((((((((((((//((((((((((
(((((((((((((((((((((((((((((((((((//((((((((((((((((((((((((((((((((//(((((((((((((((((((((((((((((
((((((((((((((((/(((((((((((((((((((((((((((((((//((((((((((((((((((((((((((((((((((((((((((((((((((
(((((((((                                        ((((((((((((((((((((((((((((((((/((((((((((((((((((
(((((((((                                            ((((((((//(((((((((((((((((/((((((((((((((/((((
(((((((//                                               ((((/((((((((((((((///(((((((((((((((//(((((
((((((/((                                                 #(((((((((((((((/(((((((((((((((((((((((((
(((((((((                                                  ###((((((((((((((((((((((((((//((((((((((
(((((((((             ######################                #####((((((((((((((((((((///((((((((((((
(((((((((             #########################              ######(((((((((((((((((((((((((((((((((
(((((((((             ##########################              ########((((((((((((((((((((((((((((//
(((((((((             ###########################             ###########((((((/(((((((((((((((//(((
(((((((((             ###########################             ##############/(((((((((((((((/(((((((
(((((((((             ###########################             #################(((((((((((((((((((((
((//(((((             ##########################              ###################(((((((((((((((((((
(((((((((             #########################              #######################((((((((((((((((
(((((((((             #######################                ##########################(((((((((((((
(((((((((             #######(#########*                    ##############################((((((((((
(((((((((                                                 ###################################(((/(((
(((((((((                                               *######################################(((((
(((((((((                                             ############################################((
(((((((((                                          #################################################
(((((((((                                   .#######################################################
(((((((((             ##############################################################################
(((((((((             #######(######################################################################
(((((((((             ##################################                          #############(####
(((((((/(             #################################.  #(####################  ,#################
(((/(/(((             #################################  #######################*  #################
(((((((((             ################################   ########################   ################
(((((((((             ##############################                                  ##############
((&((((((             ##############################   ,#                        #    ##############
(((((((((             ######(#######################  ####                      ####  ##############
((#((((((             ##############################                                  ##############
((&((((((             ###############################                               /###############
(((((((((             ###############################      ####################     (#####(#########
(/%((((((             ###############################      ####################     (###############
/((((((((((###########################################    ######################    ################
(((((((((((((/######################################################################################
((((((((((///(((/#############################################(#####################################
\n\n¡BIENVENIDO AL PARQUEADERO INTELIGENTE!\n¿Qué función desea utilizar?\n
1.REGISTRAR INGRESO DE UN AUTO\n2.CALCULAR EL TIEMPO QUE UN AUTO LLEVA PARQUEADO\n3.REGISTRAR LA SALIDA DE UN VECHÍCULO\n4.INDICAR CUANTOS ESPACIOS HAY DISPONIBLES\n5.CUANTOS AUTOS HAY PARQUEADOS HACE MÁS DE 8 HORAS\n6.SALIR"))

(define (PARQUEADERO)
  (define espacio 0)
  (define espmod 0)
  (define vechoras 0)
  (define vecplacas 0)
  (display "¿De qué tamaño desea crear su parqueadero? ")
  (set! espacio (read))
  
  )

