#lang racket


;----------------------------------------------------------------------------------------------------------------------------;
;-----------------------------------------------------Funcion constructora---------------------------------------------------;

(provide player)
                                                                     ;Funcion Constructora
(define (player  id name color wins losses draws remaining-pieces)   ;Dominio 2 string y 5 int: id (int), name(string), color(string), wins(int), losses(int) 
  (list id name color wins losses draws remaining-pieces)            ;draws (int), remaining-pieces(int)
  )                                                                  ;Recorrido una lista que contiene 2 string y 5 enteros en el orden dado en el dominio

;----------------------------------------------------------------------------------------------------------------------------;
;---------------------------------------------------------Funciones get------------------------------------------------------;

(provide getIdPlayer)

(define (getIdPlayer player)                                         ;funcion get para el primer elemento del TDA player 
  (if (null? player)                                                 ;Dominio es una lista
      (print "Error: Lista Vacia")                                   ;Recorrido es un int (id)
      (car player))                                                  
  )

(provide getNamePlayer)

(define (getNamePlayer player)                                       ;funcion get para el segundo elemento del TDA player
  (if (null? player)                                                 ;Dominio es una lista
      (print "Error: Lista Vacia")                                   ;Recorrido es un string (name)
      (car (cdr player))
      )                                            
  )                                                                  

(provide getColorPlayer)

(define (getColorPlayer player)                                      ;funcion get para el tercer elemento del TDA player
  (if (null? player)                                                 ;Dominio es una lista
      (print "Error: Lista Vacia")                                   ;Recorrido es un string (color)
      (list (car (cdr (cdr player))))
      )                                      
  )                                                                  

(provide getWinsPlayer)

(define (getWinsPlayer player)                                       ;funcion get para el cuarto elemento del TDA player
  (if (null? player)                                                 ;Dominio es una lista
      (print "Error: Lista Vacia")                                   ;Recorrido es un int (wins)
      (car (cdr (cdr (cdr player)))))                                 
  )                                                                  

(provide getLossesPlayer)

(define (getLossesPlayer player)                                     ;funcion get para el quinto elemento del TDA player
  (if (null? player)                                                 ;Dominio es una lista
      (print "Error: Lista Vacia")                                   ;Recorrido es un int (losses)
      (car (cdr (cdr (cdr (cdr player))))))                       
  )                                                                  

(provide getDrawsPlayer)

(define (getDrawsPlayer player)                                      ;funcion get para el sexto elemento del TDA player
  (if (null? player)                                                 ;Dominio es una lista
      (print "Error: Lista Vacia")                                   ;Recorrido es un int (draws)
      (car (cdr (cdr (cdr (cdr ( cdr player)))))))                       
  )                                                                  

(provide getRemainingPiecesPlayer)

(define (getRemainingPiecesPlayer player)                            ;funcion get para el septimo elemento del TDA player
  (if (null? player)                                                 ;Dominio es una lista
      (print "Error: Lista Vacia")                                   ;Recorrido es un int (RemainingPieces)
      (car (cdr (cdr (cdr (cdr (cdr (cdr player))))))))              
  )

(provide disminuir-piezas)

(define (disminuir-piezas player)                                     ;Funcion que disminuye las piezas del jugador
  (if (null? (cdr player))                                            ;Dominio: TDA Player
      (cons (- (car player) 1) null)                                  ;Recorrido: TDA Player
      (cons (car player) (disminuir-piezas (cdr player)))             ;Recursion natural
      )
)

(provide null-piezas?)

(define (null-piezas? player)                                 ;Funcion que verifica si al jugador le quedan piezas
  (if (eq? (getRemainingPiecesPlayer player) 0)               ;Dominio: TDA Player
      #t                                                      ;Recorrido: Booleano (#t y #f)
      #f                                                      
      )
  )




;----------------------------------------------------------------------------------------;
;------------------------------ Funcion player-update-stas ------------------------------;



(provide actualizar-wins) 
 
(define (actualizar-wins player)                                      ;Funcion que actualiza las victorias del jugador
  (define (aux player cont)                                           ;Dominio: TDA Player
    (if (eq? cont 4)                                                  ;Recorrido: TDA Player
        (cons (+ (car player) 1) (cdr player))                        ;Recursion natural
        (cons (car player) (aux (cdr player) (+ cont 1)))
        )
    )
  (aux player 1)
  )

(provide actualizar-losses)

(define (actualizar-losses player)                                 ;Funcion que actualiza las victorias del jugador
  (define (aux player cont)                                        ;Dominio: TDA Player
    (if (eq? cont 5)                                               ;Recorrido: TDA Player
        (cons (+ (car player) 1) (cdr player))                     ;Recursion natural
        (cons (car player) (aux (cdr player) (+ cont 1)))
        )
    )
  (aux player 1)
  )

(provide actualizar-draws)

(define (actualizar-draws player)                                   ;Funcion que actualiza las empates del jugador
 (define (aux player cont)                                          ;Dominio: TDA Player
    (if (eq? cont 6)                                                ;Recorrido: TDA Player
        (cons (+ (car player) 1) (cdr player))                      ;Recursion natural
        (cons (car player) (aux (cdr player) (+ cont 1)))
        )
    )
  (aux player 1)
  )

(provide player-update-stats)

(define (player-update-stats player result)                        ;Funcion que actualiza las estadisticas del jugador
                                                                   ;Dominio: TDA Player y un String
  (if (equal? result "win")                                        ;Recorrido: TDA Player
      (actualizar-wins player)                                     ;Recursion no aplica
      (if (equal? result "loss")
          (actualizar-losses player)
          (if (equal? result "draws")
              (actualizar-draws player)
              player
              )
          )
      )
  )

;----------------------------------------------------------------------------------------;