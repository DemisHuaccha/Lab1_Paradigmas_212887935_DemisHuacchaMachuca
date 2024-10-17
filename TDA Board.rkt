#lang racket
 
(require "TDA-Piece.rkt")
; Ejemplos para probar las funciones 
;(define red-piece (piece "red"))
;(define yellow-piece (piece "yellow"))
;(define b0(board))
;(define b1 (board-set-play-piece b0 3 red-piece))
;(define b2 (board-set-play-piece b1 3 yellow-piece)) ;Funciona bien hasta aqui;

;--------------------Funcion Constructoras --------------------------;

(define (board)                           ;TDA board, funcion constructora
  (list '() '() '() '() '() '() '())      ;Dominio/Entrada Null/Vacio
  )                                       ;Recorrido/Salida Una Lista de 7 listas vacias

;--------------------------------------------------------------------;
;---------------------- Funciones Get -------------------------------;

(define (getC1 Board)                     ;Funcion get de la columna 1
  (car Board)                             ;Dominio Board ( 1 Lista 7 listas vacias)
  )                                       ;Recorrido 1 lista de 6 elementos que en un inicio puede estar vacia.

(define (getC2 Board)                     ;Funcion get de la columna 2
  (car (cdr Board))                       ;Dominio Board ( 1 Lista 7 listas vacias)
  )                                       ;Recorrido 1 lista de 6 elementos que en un inicio puede estar vacia.


(define (getC3 Board)                     ;Funcion get de la columna 3
  (car (cdr (cdr Board)))                 ;Dominio Board ( 1 Lista 7 listas vacias)
  )                                       ;Recorrido 1 lista de 6 elementos que en un inicio puede estar vacia.


(define (getC4 Board)                     ;Funcion get de la columna 4
  (car (cdr (cdr (cdr Board))))           ;Dominio Board ( 1 Lista 7 listas vacias)
  )                                       ;Recorrido 1 lista de 6 elementos que en un inicio puede estar vacia.


(define (getC5 Board)                     ;Funcion get de la columna 5
  (car (cdr (cdr (cdr (cdr Board)))))     ;Dominio Board ( 1 Lista 7 listas vacias)
  )                                       ;Recorrido 1 lista de 6 elementos que en un inicio puede estar vacia.


(define (getC6 Board)                               ;Funcion get de la columna 6
  (car (cdr (cdr (cdr (cdr (cdr Board))))))         ;Dominio Board ( 1 Lista 7 listas vacias)
  )                                                 ;Recorrido 1 lista de 6 elementos que en un inicio puede estar vacia.


(define (getC7 Board)                               ;Funcion get de la columna 7
  (car (cdr (cdr (cdr (cdr (cdr (cdr Board)))))))   ;Dominio Board ( 1 Lista 7 listas vacias)
  )                                                 ;Recorrido 1 lista de 6 elementos que en un inicio puede estar vacia.


(define (getF1 Columna)                             ;Funcion get del primer elemento en una columna
  (if (null? Columna)                               ;Dominio Columna( 1 Lista 7 listas vacias)
      (print "Error: Columna Vacia")                ;Recorrido 1 elemento tipo piece
      (car Columna))                                     
  )                                                 

(define (getF2 Columna)                             ;Funcion get del segundo elemento en una columna
  (if (null? Columna)                               ;Dominio Columna( 1 Lista 7 listas vacias)
      (print "Error: Columna Vacia")                ;Recorrido 1 elemento tipo piece
      (car (cdr Columna)))
  )

(define (getF3 Columna)                             ;Funcion get del tercer elemento en una columna
  (if (null? Columna)                               ;Dominio Columna( 1 Lista 7 listas vacias)
      (print "Error: Columna Vacia")                ;Recorrido 1 elemento tipo piece
      (car (cdr (cdr Columna))))
  )

(define (getF4 Columna)                             ;Funcion get del cuarto elemento en una columna
  (if (null? Columna)                               ;Dominio Columna( 1 Lista 7 listas vacias)
      (print "Error: Columna Vacia")                ;Recorrido 1 elemento tipo piece
      (car (cdr (cdr (cdr Columna)))))
  )

(define (getF5 Columna)                             ;Funcion get del quinto elemento en una columna
  (if (null? Columna)                               ;Dominio Columna( 1 Lista 7 listas vacias)
      (print "Error: Columna Vacia")                ;Recorrido 1 elemento tipo piece
      (car (cdr (cdr (cdr (cdr Columna))))))        
  )

(define (getF6 Columna)                             ;Funcion get del sexto elemento en una columna
  (if (null? Columna)                               ;Dominio Columna( 1 Lista 7 listas vacias)
      (print "Error: Columna Vacia")                ;Recorrido 1 elemento tipo piece
      (car (cdr (cdr (cdr (cdr (cdr Columna)))))))  
  )

;--------------------------------------------------------------------------------------------------------;
;-----------------------------------Funciones Auxiliares del TDA Board-----------------------------------;

(define (member? x L)                                          ;Funcion auxiliar general, temporalmente en este archivo
  (cond                                                        ;Dominio un int y una lista, Recorrido (#t / #f)
    ((null? L) #f)                                             ;Recursion de cola
    ((eq? x (car L)) #t)
    (else (member? x (cdr L)))
    )
  )

(define (buscar-getC Column Board)                             ;Funcion que busca que columna se esta pidiento y la entrega
  (cond                                                        ;Dominio un int (Column) que es la columna que se pide y un board (Tablero) 
    ((eq? 1 Column) (getC1 Board))                             ;Recorrido una Columna (Ques es una lista)
    ((eq? 2 Column) (getC2 Board))                             ;Recursion de cola
    ((eq? 3 Column) (getC3 Board))
    ((eq? 4 Column) (getC4 Board))
    ((eq? 5 Column) (getC5 Board))
    ((eq? 6 Column) (getC6 Board))
    ((eq? 7 Column) (getC7 Board))
    (else "Columna inexistente")
    )
   )
                                                                                ;Recursion de cola                                  
                                                                                ;Funcion que busca la posicion libre mas baja en la columna para colocar el TDA Piece
(define (buscar-posicion-en-columna column contador)                            ;Dominio columna (list) y contador (int)
  (cond                                                                         ;Recorrido int (contador)
    ((and (not (null? column)) (eq? contador 1) (not(eq? (car column) 0))) (print "Columna llena"))
    ((and (null? column) (eq? contador 1)) 7 )                                  
    ((and (eq? (car column) 0) (not (eq? (car (cdr column)) 0))) contador )  
    (else (buscar-posicion-en-columna (cdr column) (+ contador 1)))
    )
  )

(define (colocar-piece column piece posicion posicion-act)                                          ;Recursion de cola
  (cond                                                                                             ;Funcion que coloca un TDA piece en una columna en la posicion dada
    ((and (null? column) (eq? posicion-act 1)) (list 0 0 0 0 0 piece))                              ;Dominio Columna (list), piece (TDA piece), posicion (int), posicion-act (int)
    ((and (eq? posicion posicion-act) (null? (cdr column))) piece)                                  ;recorrido list (Columna)
    ((and (eq? posicion posicion-act) (not (null? (cdr column)))) (cons piece (cdr column)))        
    (else  (cons (car column) (colocar-piece (cdr column) piece posicion (+ posicion-act 1))))
    )
  
  )


;Recursion de cola
;Funcion que actualiza la columna en el tablero
;Dominio Columna (list), piece (TDA piece), posicion (int), posicion-act (int)
;recorrido list (Columna)

(define (colocar-columna-en-tablero column board posicion posicion-act)                                                      
  (cond                                                                                                                      
    ((and (eq? posicion posicion-act) (null? (cdr board)) (eq? posicion-act 7)) column)                                                           
    ((and (eq? posicion posicion-act) (not (null? (cdr board)))) (cons column (cdr board)))                      
    (else (cons (car board) (colocar-columna-en-tablero column (cdr board) posicion (+ posicion-act 1))))
    )
  )


;----------------------------------------------------------------------------------------------------------------------------;
;-----------------------------------------------Funcion Board-can-play-------------------------------------------------------;

(define (board-can-play? board)                                 ;Funcion que verfica si existe una posicion libre en el tablero
  (cond                                                        ;Dominio Board
    ((null? board) #f)                                         ;Recorrido #t si existe una posicion libre en el tablero o #f 
    ((null? (getC1 board)) #t)                                 ;en el caso contrario
    ((member? 0 (getC1 board)) #t)                             ;Recursion de cola
    (else (board-can-play? (cdr board)))
    )
  )


;----------------------------------------------------------------------------------------------------------------------------;
;-----------------------------------------------Funcion Board-set-play-piece-------------------------------------------------;
(define (board-set-play-piece board column piece)
  (colocar-columna-en-tablero (colocar-piece (buscar-getC column board) piece (buscar-posicion-en-columna (buscar-getC column board) 1) 1) board column 1)
  )




