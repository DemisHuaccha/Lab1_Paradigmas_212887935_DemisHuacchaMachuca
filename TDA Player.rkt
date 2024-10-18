#lang racket


(provide player)
                                                                     ;Funcion Constructora
(define (player  id name color wins losses draws remaining-pieces)   ;Dominio 2 string y 5 int: id (int), name(string), color(string), wins(int), losses(int) 
  (list id name color wins losses draws remaining-pieces)            ;draws (int), remaining-pieces(int)
  )                                                                  ;Recorrido una lista que contiene 2 string y 5 enteros en el orden dado en el dominio

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
      (car (cdr player)))                                            
  )                                                                  

(provide getColorPlayer)

(define (getColorPlayer player)                                      ;funcion get para el tercer elemento del TDA player
  (if (null? player)                                                 ;Dominio es una lista
      (print "Error: Lista Vacia")                                   ;Recorrido es un string (color)
      (car (cdr (cdr player))))                                      
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

