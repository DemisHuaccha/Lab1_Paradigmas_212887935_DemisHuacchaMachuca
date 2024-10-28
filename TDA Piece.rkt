#lang racket


;----------------------------------------------------------------------------------------------------------------------------;
;-----------------------------------------------------Funcion constructora---------------------------------------------------;

(provide piece)

(define (piece color)                                 ;Funcion Constructora del TDA Piece
  (list color)                                        ;Dominio 1 string (color)
  )                                                   ;Recorrido es una lista 


;----------------------------------------------------------------------------------------------------------------------------;
;---------------------------------------------------------Funciones get------------------------------------------------------;

(provide getColor)

(define (getColor piece)                              ;Funcion get
  (if (null? piece)                                   ;Dominio 1 lista (TDA Piece)
      (print "Error: Lista Vacia")                    ;Recorrido 1 string (color)
      (car piece))
  )