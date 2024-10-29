#lang racket


(require "TDA-Piece.rkt")
(require "TDA-Player.rkt")
(require "TDA-Board.rkt")

(provide game)

 ;             "TDA P"    "TDA P"  "TDA B"    "INT"             ;Funcion constructora del TDA Game
(define (game  player1    player2   board   current-turn)       ;Dominio: 2 TDA-Player, 1 TDA-Board, 1 int y 1 lista que es game-history
  (list player1 player2 board current-turn '())                 ;Recorrido: 1 lista que contiene los elementos de entrada
  )                                                             ;No aplica recursion


;--------------------------------------------------------------------;
;------------------------- Funciones Get ----------------------------;

(provide getGamePlayer1)

(define (getGamePlayer1 game)                                   ;Funcion get de player1
  (car game)                                                    ;Dominio: TDA Game
  )                                                             ;Recorrido: TDA Player

(provide getGamePlayer2)

(define (getGamePlayer2 game)                                   ;Funcion get de player2
  (car (cdr game))                                              ;Dominio: TDA Game
  )                                                             ;Recorrido: TDA Player

(provide gamegetboard)

(define (gamegetboard game)                                     ;Funcion get de board
  (car (cdr (cdr game)))                                        ;Dominio: TDA Game
  )                                                             ;Recorrido: TDA Board

(provide getGameCurrentTurn)

(define (getGameCurrentTurn game)                               ;Funcion get de current turn
  (car (cdr (cdr (cdr game))))                                  ;Dominio: TDA Game
  )                                                             ;Recorrido: Un Int


;----------------------------------------------------------------------------------------;
;--------------------------------- Funcion game-history ---------------------------------;


;Funcion game-history
;Dominio: TDA Game
;Recorrido: una lista game-history

(provide game-history)

(define (game-history game)
  (car (cdr (cdr (cdr (cdr game)))))
  )

;Funcion game-historyaux
;Dominio: TDA Game , columna (int), color (string)
;Recorrido: una lista game-history

(provide game-historyaux)

(define (game-historyaux gamehistory columna color)
  (cond
    ((null? gamehistory) (cons (cons columna color) null))
    (else (cons (car gamehistory) (game-historyaux (cdr gamehistory) columna color)))
    )
  )

;----------------------------------------------------------------------------------;
;------------------------------ Funcion game-is-draw ------------------------------;

;Funcion 
;Dominio: TDA Game
;Recorrido: boolean (#t o #f)
;Recursion no aplica

(provide game-is-draw?)

(define (game-is-draw? game)
  (if (null? game)
      #f
      (if (and (eq? (getRemainingPiecesPlayer (getGamePlayer1 game)) 0) (eq? (getRemainingPiecesPlayer (getGamePlayer2 game)) 0))      
          #t                                                         
          (if (board-can-play? (gamegetboard game))
              #f
              #t
              )
          )
      )
  )
;--------------------------------------------------------------------------------------------;
;------------------------------ Funcion game-get-current-player ------------------------------;

(provide game-get-current-player)

(define (game-get-current-player game)                                                             ;Funcion que entrega el jugador del turno actual
  (if (eq? (getIdPlayer (getGamePlayer1 game))(getGameCurrentTurn game))                           ;Dominio TDA Game
      (getGamePlayer1 game)                                                                        ;Recorrido TDA Player
      (getGamePlayer2 game) ;Supuniendo que el currentTurn se identifica por el id del player      ;Recursion no aplica
      )                     ;significa que solo deberian existir estos 2 casos
  )



;----------------------------------------------------------------------------------------;
;------------------------------ Funcion game-get-board ------------------------------;

(provide game-get-board)

(define (game-get-board game)                                     ;Funcion que entrega una representacion del mapa
  (gamegetboard game)                                   ;Dominio: TDA Game
  )                                                               ;Recorrido: print TDA Board


;----------------------------------------------------------------------------------------;
;--------------------------------- Funcion game-set-end ---------------------------------;

;Funcion que finaliza el juego y actualiza las estadisticas de los jugadores dentro del juego
;Dominio: TDA Game
;Recorrido: TDA Game

(provide game-set-end)

(define (game-set-end game)
  
  (if (eq? (board-who-is-winner (gamegetboard game)) (car (getColorPlayer (getGamePlayer1 game))))
      (list  (player-update-stats (getGamePlayer1 game) "win") (player-update-stats (getGamePlayer2 game) "loss") (gamegetboard game) (getGameCurrentTurn game)(game-history game))
      (if (eq? (board-who-is-winner (gamegetboard game)) (car (getColorPlayer (getGamePlayer2 game))))
          (list  (player-update-stats (getGamePlayer1 game) "loss") (player-update-stats (getGamePlayer2 game) "win") (gamegetboard game) (getGameCurrentTurn game) (game-history game) )
          (list  (player-update-stats (getGamePlayer1 game) "draws") (player-update-stats (getGamePlayer2 game) "draws") (gamegetboard game) (getGameCurrentTurn game) (game-history game) )
          )
      )
  )

;---------------------------------------------------------------------------------------------;
;--------------------------------- Funcion game-player-set-move ------------------------------;

(provide game-player-set-move)


(define (comprobacion game)                                                                ;Funcion que comprueba si el juego se ha terminado
    (if (game-is-draw? game)                                                               ;Dominio: TDA Game
        (game-set-end game)                                                                ;Recorrido: TDA Game
        (if (eq? (board-who-is-winner (gamegetboard game)) 0)                              ;Recursion no aplica
            game
            (game-set-end game)
            )
        )
    )

                                                                                          ;Funcion que realiza la jugada
;                            TDA-G    TDA-P     Int                                       ;Dominio: TDA Game , TDA Player y un Int
(define (game-player-set-move  game    player   column)                                   ;Recorrido: TDA Game
  (if (not (eq? (getIdPlayer (game-get-current-player game)) (getIdPlayer player)))       ;Recursion no aplica
      (display "No es el turno de este jugador.")
      (if (null-piezas? player)
          (display "Juegador sin piezas")
          (if (eq? (getIdPlayer (getGamePlayer1 game)) (getIdPlayer player)) 
              (comprobacion (list (disminuir-piezas (getGamePlayer1 game)) (getGamePlayer2 game) (board-set-play-piece (gamegetboard game) column (getColorPlayer player)) (getIdPlayer (getGamePlayer2 game)) (game-historyaux (game-history game) column (getColorPlayer player)) ))
              (comprobacion (list (getGamePlayer1 game) (disminuir-piezas (getGamePlayer2 game)) (board-set-play-piece (gamegetboard game) column (getColorPlayer player)) (getIdPlayer (getGamePlayer1 game)) (game-historyaux (game-history game) column (getColorPlayer player)) ))
              )
          )
      )
  )



;-------------------------------------------------------------------------------------------------;
