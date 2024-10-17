#lang racket

(provide piece)

(define (piece color)                                 ;Funcion Constructora del TDA Piece
  (list color)                                        ;Dominio 1 string (color)
  )                                                   ;Recorrido es una lista 

(provide getColor)

(define (getColor piece)                              ;Funcion get
  (if (null? piece)                                   ;Dominio 1 lista (TDA Piece)
      (print "Error: Lista Vacia")                    ;Recorrido 1 string (color)
      (car piece))
  )