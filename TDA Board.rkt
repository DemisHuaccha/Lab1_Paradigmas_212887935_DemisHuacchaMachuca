#lang racket

(require "TDA-Piece.rkt")
(require "TDA-Player.rkt")

; Ejemplos para probar las funciones 
;(define red-piece (piece "red"))
;(define yellow-piece (piece "yellow"))
;(define b0(board))
;(define b1 (board-set-play-piece b0 3 red-piece))
;(define b2 (board-set-play-piece b1 3 yellow-piece))
;(define a (list (list 0 0 "hola" "hola" "adios" "hola")(list 0 0 0 0 "hola" "hola")(list 0 0 0 0 "hola" "hola") (list 0 0 0 0 "hola" "hola") (list 0 0 0 0 "hola" "hola") (list 0 0 0 0 "hola" "hola") (list 0 0 "hola" "hola" "hola" "hola"))) 
;Funciona bien hasta aqui;
;--------------------Funcion Constructoras --------------------------;

(provide board)

(define (board)                           ;TDA board, funcion constructora
  (list '() '() '() '() '() '() '())      ;Dominio/Entrada Null/Vacio
  )                                       ;Recorrido/Salida Una Lista de 7 listas vacias

;--------------------------------------------------------------------;
;---------------------- Funciones Get -------------------------------;

(provide getC1)

(define (getC1 Board)                     ;Funcion get de la columna 1
  (car Board)                             ;Dominio Board ( 1 Lista 7 listas vacias)
  )                                       ;Recorrido 1 lista de 6 elementos que en un inicio puede estar vacia.

(provide getC2)

(define (getC2 Board)                     ;Funcion get de la columna 2
  (car (cdr Board))                       ;Dominio Board ( 1 Lista 7 listas vacias)
  )                                       ;Recorrido 1 lista de 6 elementos que en un inicio puede estar vacia.

(provide getC3)

(define (getC3 Board)                     ;Funcion get de la columna 3
  (car (cdr (cdr Board)))                 ;Dominio Board ( 1 Lista 7 listas vacias)
  )                                       ;Recorrido 1 lista de 6 elementos que en un inicio puede estar vacia.

(provide getC4)

(define (getC4 Board)                     ;Funcion get de la columna 4
  (car (cdr (cdr (cdr Board))))           ;Dominio Board ( 1 Lista 7 listas vacias)
  )                                       ;Recorrido 1 lista de 6 elementos que en un inicio puede estar vacia.

(provide getC5)

(define (getC5 Board)                     ;Funcion get de la columna 5
  (car (cdr (cdr (cdr (cdr Board)))))     ;Dominio Board ( 1 Lista 7 listas vacias)
  )                                       ;Recorrido 1 lista de 6 elementos que en un inicio puede estar vacia.

(provide getC6)

(define (getC6 Board)                               ;Funcion get de la columna 6
  (car (cdr (cdr (cdr (cdr (cdr Board))))))         ;Dominio Board ( 1 Lista 7 listas vacias)
  )                                                 ;Recorrido 1 lista de 6 elementos que en un inicio puede estar vacia.

(provide getC7)

(define (getC7 Board)                               ;Funcion get de la columna 7
  (car (cdr (cdr (cdr (cdr (cdr (cdr Board)))))))   ;Dominio Board ( 1 Lista 7 listas vacias)
  )                                                 ;Recorrido 1 lista de 6 elementos que en un inicio puede estar vacia.

(provide getF1)

(define (getF1 Columna)                             ;Funcion get del primer elemento en una columna
  (if (null? Columna)                               ;Dominio Columna( 1 Lista 7 listas vacias)
      '()                                           ;Recorrido 1 elemento tipo piece
      (car Columna))                                     
  )

(provide getF2)

(define (getF2 Columna)                             ;Funcion get del segundo elemento en una columna
  (if (null? Columna)                               ;Dominio Columna( 1 Lista 7 listas vacias)
      '()                                           ;Recorrido 1 elemento tipo piece
      (car (cdr Columna)))
  )

(provide getF3)

(define (getF3 Columna)                             ;Funcion get del tercer elemento en una columna
  (if (null? Columna)                               ;Dominio Columna( 1 Lista 7 listas vacias)
      '()                                           ;Recorrido 1 elemento tipo piece
      (car (cdr (cdr Columna))))
  )

(provide getF4)

(define (getF4 Columna)                             ;Funcion get del cuarto elemento en una columna
  (if (null? Columna)                               ;Dominio Columna( 1 Lista 7 listas vacias)
      '()                                           ;Recorrido 1 elemento tipo piece
      (car (cdr (cdr (cdr Columna)))))
  )

(provide getF5)

(define (getF5 Columna)                             ;Funcion get del quinto elemento en una columna
  (if (null? Columna)                               ;Dominio Columna( 1 Lista 7 listas vacias)
      '()                                           ;Recorrido 1 elemento tipo piece
      (car (cdr (cdr (cdr (cdr Columna))))))        
  )

(provide getF6)

(define (getF6 Columna)                             ;Funcion get del sexto elemento en una columna
  (if (null? Columna)                               ;Dominio Columna( 1 Lista 7 listas vacias)
      '()                                           ;Recorrido 1 elemento tipo piece
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

(provide buscar-getC)

(define (buscar-getC Column Board)                             ;Funcion que busca que columna se esta pidiento y la entrega
  (cond                                                        ;Dominio un int (Column) que es la columna que se pide y un board (Tablero) 
    ((eq? 0 Column) (getC1 Board))                             ;Recorrido una Columna (Ques es una lista)
    ((eq? 1 Column) (getC2 Board))                             ;Recursion de cola
    ((eq? 2 Column) (getC3 Board))
    ((eq? 3 Column) (getC4 Board))
    ((eq? 4 Column) (getC5 Board))
    ((eq? 5 Column) (getC6 Board))
    ((eq? 6 Column) (getC7 Board))
    (else "Columna inexistente")
    )
   )

(define (buscar-getF Fila Board)                             ;Funcion que busca que columna se esta pidiento y la entrega
  (cond                                                        ;Dominio un int (Column) que es la columna que se pide y un board (Tablero) 
    ((eq? 0 Fila) (getF1 (getC1 Board)))                            ;Recorrido una Columna (Ques es una lista)
    ((eq? 1 Fila) (getF2 (getC1 Board)))                            ;Recursion de cola
    ((eq? 2 Fila) (getF3 (getC1 Board)))
    ((eq? 3 Fila) (getF4 (getC1 Board)))
    ((eq? 4 Fila) (getF5 (getC1 Board)))
    ((eq? 5 Fila) (getF6 (getC1 Board)))
    (else "Columna inexistente")
    )
   )

(provide buscar-posicion-en-columna)
                                                                                ;Recursion de cola                                  
                                                                                ;Funcion que busca la posicion libre mas baja en la columna para colocar el TDA Piece
(define (buscar-posicion-en-columna column contador)                            ;Dominio columna (list) y contador (int)
  (cond                                                                         ;Recorrido int (contador)
    ((and (not (null? column)) (eq? contador 0) (not(eq? (car column) 0))) (print "Columna llena"))
    ((and (null? column) (eq? contador 0)) 6 )                                  
    ((and (eq? (car column) 0) (not (eq? (car (cdr column)) 0))) contador )  
    (else (buscar-posicion-en-columna (cdr column) (+ contador 1)))
    )
  )

(provide colocar-piece)

(define (colocar-piece column piece posicion posicion-act)                                          ;Recursion de cola
  (cond                                                                                             ;Funcion que coloca un TDA piece en una columna en la posicion dada
    ((and (null? column) (eq? posicion-act 0)) (list 0 0 0 0 0 (car piece)))                              ;Dominio Columna (list), piece (TDA piece), posicion (int), posicion-act (int)
    ((and (eq? posicion posicion-act) (null? (cdr column))) (car piece))                                  ;recorrido list (Columna)
    ((and (eq? posicion posicion-act) (not (null? (cdr column)))) (cons (car piece) (cdr column)))        
    (else  (cons (car column) (colocar-piece (cdr column) piece posicion (+ posicion-act 1))))
    )
  
  )

(provide colocar-columna-en-tablero)

;Recursion de cola
;Funcion que actualiza la columna en el tablero
;Dominio Columna (list), piece (TDA piece), posicion (int), posicion-act (int)
;recorrido list (Columna)

(define (colocar-columna-en-tablero column board posicion posicion-act)                                                      
  (cond                                                                                                                      
    ((and (eq? posicion posicion-act) (null? (cdr board)) (eq? posicion-act 6)) column)                                                           
    ((and (eq? posicion posicion-act) (not (null? (cdr board)))) (cons column (cdr board)))                      
    (else (cons (car board) (colocar-columna-en-tablero column (cdr board) posicion (+ posicion-act 1))))
    )
  )

;Funcion que crea todas las diagonales posibles y luego las filtra para entregar solo aquellas que tienen un tamaño mayor a 4
;Dominio: TDA Board
;Recorrido: Una lista con las diagonales (lista de listas)
;Recursion natural
;Apoyo de chatgpt para la construccion de esta funcion

(define (diagonales Board)
  (define (extraer-diagonal i j delta-i delta-j)                                                  ;delta- y delta-j son para direccionar el loop
    (define (extraer-diagonalaux i j acum)                                                                       ;Funcion que crea un loop para generar una diagonal hasta llegar al limite del board 
      (if (or (< i 0) (>= i (length Board))(< j 0) (>= j (length (list-ref Board i))))   
          (reverse acum)                                                                          
          (extraer-diagonalaux (+ i delta-i) (+ j delta-j) (cons (list-ref (list-ref Board i) j) acum))          ;Llamada recursiva que crea la diagonal.
          )
      )
    (extraer-diagonalaux i j '())
    )

  (define (diagonales-en-direccion delta-i delta-j)                                                ;Funcion Diagonales-en-dirrecion funcion cascaron para tener disponibles los datos de dirrecion de las diagonales
    (define (diagonales-desde j)                                                                   ;Funcion Diagonales-desde funcion que recorre todas las diagonales a partir de una la posicion de una fila 
      (define (diagonalesaux i j acum2)                                                                    ;Funcion que crea un loop para recorrer el Board en todas las posiciones posibles
        (if (or (< i 0) (>= i (length Board))(< j 0) (>= j (length (list-ref Board i))))
            (reverse acum2)                                                                        
            (diagonalesaux (+ i delta-i) (+ j delta-j)(cons (extraer-diagonal i j delta-i delta-j) acum2))
            )
        )
      (diagonalesaux 0 j '())
      )
    (apply append (map diagonales-desde (range (length (car Board)))))                            ;Funcion apply, utilizada para unir en una sola lista los elementos (diagonales que son listas) de listas generadas por el loop
    )
  
  (define todas-diagonales                   ; Funcion que une todas las diagonales obtenidas
    (append (diagonales-en-direccion 1 -1)  ; Abajo-izquierda
            (diagonales-en-direccion 1 1)   ; Abajo-derecha
            (diagonales-en-direccion -1 -1)  ; Arriba-izquierda
            (diagonales-en-direccion -1 1))  ; Arriba-derecha
    )

  (filter (lambda (d) (>= (length d) 4)) todas-diagonales) ;Filtra y entrega todas las diagonales que tienen un tamaño mayor o igual a 4
  )

;----------------------------------------------------------------------------------------------------------------------------:

(define (rC columna)                                                              ;Funcion que verifica que sea una lista vacia
    (cond                                                                         ;Dominio: lista
      ((eq? '() columna) (list 0 0 0 0 0 0))                                      ;Recorrido: lista
      (else columna)                                      
      )
    )

(provide rellenarColumna)

(define (rellenarColumna board)                                                        ;Funcion que une las columnas rellenadas
  (cond                                                                                ;Dominio: TDA Board 
    ((null? (cdr board)) (cons (rC (car board)) null))                                 ;Recorrido: TDA Board
    (else (cons (rC (car board)) (rellenarColumna (cdr board))))                       ;Recursion natural
    )
  )

;----------------------------------------------------------------------------------------------------------------------------;
;-----------------------------------------------Funcion Board-can-play-------------------------------------------------------;

(provide board-can-play?)

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

;Funcion que coloca una pieza en el fondo de la columna selecionada
;Dominio: TDA Board, int (columna), TDA Piece
;Recorrido: TDA Board
;Recursion no aplica

(provide board-set-play-piece)

(define (board-set-play-piece board column piece)
  (if (board-can-play? board)
      (if (number?(buscar-posicion-en-columna (buscar-getC column board) 0))
          (colocar-columna-en-tablero (colocar-piece (buscar-getC column board) piece (buscar-posicion-en-columna (buscar-getC column board) 0) 0) board column 0)
          (display " No hay mas jugadas posibles en la columna"))
      (display "No hay mas jugadas posibles en la columna")
      )
  )

;----------------------------------------------------------------------------------------------------------------------------;
;----------------------------------------------Funcion Board-check-vertical-win----------------------------------------------;

(provide repetido4)

(define (repetido4 columna)
                                                                            ;Apoyo de chatgpt para la construccion de esta funcion
  (define (repetido4aux columna act)                                        ;Funcion repetido4aux es una funcion auxiliar
    (cond                                                                   ;Dominio: una lista y un string
      ((null? (cdr columna)) 1)                                             ;Recorrido: un int
      ((eq? (car columna) act) (+ 1 (repetido4aux (cdr columna) act)))      ;Recursion natural
      (else 0)                                                              ;Funcion que verifica si en una columna existe un dato distinto de 0 que se repita consecutivamente por lo menos 4 veces 
      )
    )

  (if (null? columna)                                                       ;Funcion repetido4
      0                                                                     ;Dominio: una lista
      (if (eq? (car columna) 0)                                             ;Recorrido: string o un int (0)
          (repetido4 (cdr columna))                                         ;Recursion de cola
          (if (>= (repetido4aux columna (car columna)) 4)                   ;Funcion que verifica que se haya repetido 4 veces por lo menos el primer elemento de la lista hasta que solo un elemento en la lista
              (car columna)
              (repetido4 (cdr columna)))
          )
      )
  )

(provide board-check-vertical-win)

(define (board-check-vertical-win board)                               ;Funcion board-check-vertical-win 
  (cond                                                                ;Dominio: un TDA board
    ((null? board) 0)                                                  ;Recorrido: un string o un int (0)
    ((string? (repetido4 (getC1 board))) (repetido4 (car board)))      ;Recursion de cola para recorrer tablero, recursion natural para verificar que existan 4 piezas iguiales consecutivas
    (else (board-check-vertical-win (cdr board)))                      ;
     )
  )

;----------------------------------------------------------------------------------------------------------------------------;
;----------------------------------------------Funcion Board-check-horizontal-win----------------------------------------------;

(define (F board contador)                                                             ;Funcion que crear las filas
    (cond                                                                              ;Dominio: TDA Board y un Int
      ((null? board) null)                                                             ;Recorrido: lista 
      ((eq? '() (buscar-getF contador board)) (cons 0 (F (cdr board) contador)))       ;Recursion natural
      (else (cons (buscar-getF contador board) (F (cdr board) contador)))
      )
    )

(provide crearFilas)

(define (crearFilas board cont)                                                        ;Funcion que une las filas creadas
  (cond                                                                                ;Dominio: TDA Board y un Int
    ((eq? cont 6) null)                                                                ;Recorrido TDA Board, pero hecho con las filas
    (else (cons (F board cont) (crearFilas board (+ cont 1))))                         ;Recursion natural
    )
  )

(provide board-check-horizontal-win)

(define (board-check-horizontal-win board)
  (define board2 (crearFilas board 0))
 (cond                                                                  ;Dominio: un TDA board
    ((null? board2) 0)                                                  ;Recorrido: un string o un int (0)
    ((string? (repetido4 (getC1 board2))) (repetido4 (car board2)))     ;Recursion de cola para recorrer tablero, recursion natural pata verificar que existan 4 piezas iguales consecutivamente
    (else (board-check-vertical-win (cdr board2)))                      ;
     )
  )


;----------------------------------------------------------------------------------------------------------------------------;
;----------------------------------------------Funcion Board-check-diagonal-win----------------------------------------------;
(provide board-check-diagonal-win)

;Funcion que verifica que hay una pieza que se repita por lo menis 4 veces
;Dominio:TDA board
;Recorrido: string(temp)
;Recursion de cola


(define (board-check-diagonal-win board)
  
  (define (check-aux Diagonales)
    (if (null? Diagonales)                                     
        0                                                     
        (if (string? (repetido4 (car Diagonales)))            
            (repetido4 (car Diagonales))
            (check-aux (cdr Diagonales))
            )
        )
    )
  
  (check-aux (diagonales (rellenarColumna board)))
  )
  


;----------------------------------------------------------------------------------------------------------------------------;
;------------------------------------------------Funcion Board-who-is-winner-------------------------------------------------;

(provide board-who-is-winner)

(define (board-who-is-winner board)                                               ;Funcion que verifica si existe una pieza que se repita consecuitavamente por lo menos 4 veces en las dirreciones vertical, horizontal y diagonal
  (if (not (eq? (board-check-vertical-win board) 0))                              ;Dominio:TDA Board
      (board-check-vertical-win board)                                            ;Recorrido:int/string(string temporalmente)
      (if (not (eq? (board-check-horizontal-win board) 0))                        ;Recursion no aplica
          (board-check-horizontal-win board)
          (if (not (eq? (board-check-diagonal-win board) 0))
              (board-check-diagonal-win board)
              0
              )
          )
      )
  )

  


;----------------------------------------------------------------------------------------------------------------------------;



