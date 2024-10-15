#lang racket
; ejemplo para probar las funciones con un tablero lleno
;(define (board)
;  (list '(11 12 13 14 15 16) '(21 22 23 24 25 26) '(31 32 33 34 35 36) '(41 42 43 44 45 46) '(51 52 53 54 55 56) '(61 62 63 64 65 66 67) '(71 72 73 74 75 76))
; )

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

(define (member? x L)
  (cond
    ((null? L) #f)
    ((eq? x (car L)) #t)
    (else (member? x (cdr L)))
    )
  )


(define (board-can-play board)                                 ;Funcion que verfica si existe una posicion libre en el tablero
  (cond                                                        ;Dominio Board
    ((null? board) #f)                                         ;Recorrido #t si existe una posicion libre en el tablero o #f 
    ((null? (getC1 board)) #t)                                 ;en el caso contrario
    ((member? 0 (getC1 board)) #t)
    (else (board-can-play (cdr board)))
    )
  )


(define (buscar-getC Column Board)
  (cond 
    ((eq? 1 Column) (getC1 Board))
    ((eq? 2 Column) (getC2 Board))
    ((eq? 3 Column) (getC3 Board))
    ((eq? 4 Column) (getC4 Board))
    ((eq? 5 Column) (getC5 Board))
    ((eq? 6 Column) (getC6 Board))
    ((eq? 7 Column) (getC7 Board))
    (else "Columna inexistente")
    )
   )
    
;
;(define (board-set-play-piece board column piece)
;  ()
;
;  )
