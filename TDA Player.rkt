#lang racket

(define (player  id name color wins losses draws remaining-pieces)   ;Dominio 2 string y 5 int: id (int), name(string), color(string), wins(int), losses(int) 
  (list id name color wins losses draws remaining-pieces)            ;draws (int), remaining-pieces(int)
  )                                                                  ;Recorrido una lista que contiene 2 string y 5 enteros en el orden dado en el dominio

(define (getIdPlayer player)                                         ;funcion get para el primer elemento del TDA player 
  (car player)                                                       ;Dominio es una lista
  )                                                                  ;Recorrido es un int (id)

(define (getNamePlayer player)                                       ;funcion get para el segundo elemento del TDA player
  (car (cdr player))                                                 ;Dominio es una lista
  )                                                                  ;Recorrido es un string (name)

(define (getColorPlayer player)                                      ;funcion get para el tercer elemento del TDA player
  (car (cdr (cdr player)))                                           ;Dominio es una lista
  )                                                                  ;Recorrido es un string (color)

(define (getWinsPlayer player)                                       ;funcion get para el cuarto elemento del TDA player
  (car (cdr (cdr (cdr player))))                                     ;Dominio es una lista
  )                                                                  ;Recorrido es un int (wins)

(define (getLossesPlayer player)                                     ;funcion get para el quinto elemento del TDA player
  (car (cdr (cdr (cdr (cdr player)))))                               ;Dominio es una lista
  )                                                                  ;Recorrido es un int (losses)

(define (getDrawsPlayer player)                                      ;funcion get para el sexto elemento del TDA player
  (car (cdr (cdr (cdr (cdr ( cdr player))))))                        ;Dominio es una lista
  )                                                                  ;Recorrido es un int (draws)

(define (getRemainingPiecesPlayer player)                            ;funcion get para el septimo elemento del TDA player
  (car (cdr (cdr (cdr (cdr (cdr (cdr player)))))))                   ;Dominio es una lista 
  )                                                                  ;Recorrido es un int (RemainingPieces)

