#lang racket

(require "TDA-Piece.rkt")
(require "TDA-Player.rkt")
(require "TDA-Board.rkt")

 ;             "TDA P"    "TDA P"  "TDA B"    "INT"             ;Funcion constructora del TDA Game
(define (game  player1    player2   board   current-turn)       ;Dominio: 2 TDA-Player, 1 TDA-Board y 1 int
  (list player1 player2 board current-turn)                     ;Recorrido: 1 lista que contiene los elementos de entrada
  )                                                             ;No aplica recursion



;--------------------------------------------------------------------;
;------------------------- Funciones Get ----------------------------;

(define (getGamePlayer1 game)                                   ;Funcion get de player1
  (car game)                                                    ;Dominio: TDA Game
  )                                                             ;Recorrido: TDA Player

(define (getGamePlayer2 game)                                   ;Funcion get de player2
  (car (cdr game))                                              ;Dominio: TDA Game
  )                                                             ;Recorrido: TDA Player

(define (gamegetboard game)                                     ;Funcion get de board
  (car (cdr (cdr game)))                                        ;Dominio: TDA Game
  )                                                             ;Recorrido: TDA Board

(define (getGameCurrentTurn game)                               ;Fincion get de current turn
  (car (cdr (cdr (cdr game))))                                  ;Dominio: TDA Game
  )                                                             ;Recorrido: Un Int

;----------------------------------------------------------------------------------;
;------------------------------ Funcion game-is-draw ------------------------------;

;Funcion 
;Dominio: TDA Game
;Recorrido: boolean (#t o #f)
;Recursion no aplica

(define (game-is-draw? game)                                                                                                         
  (if (and (eq? (getRemainingPiecesPlayer (getGamePlayer1 game)) 0) (eq? (getRemainingPiecesPlayer (getGamePlayer2 game)) 0))      
      #t                                                         
      (if (board-can-play? (game-get-board game))
          #f
          #t
          )
      )
  )

;--------------------------------------------------------------------------------------------;
;------------------------------ Funcion game-get-current-player ------------------------------;


(define (game-get-current-player game)                                                             ;Funcion que entrega el jugador del turno actual
  (if (eq? (getIdPlayer (getGamePlayer1 game))(getGameCurrentTurn game))                           ;Dominio TDA Game
      (getGamePlayer1 game)                                                                        ;Recorrido TDA Player
      (getGamePlayer2 game) ;Supuniendo que el currentTurn se identifica por el id del player      ;Recursion no aplica
      )                     ;solo deberian existir estos 2 casos
  )



;----------------------------------------------------------------------------------------;
;------------------------------ Funcion game-get-board ------------------------------;

(define (game-get-board game)                                     ;Funcion que entrega una representacion del mapa
  (display (gamegetboard game))                                   ;Dominio: TDA Game
  )                                                               ;Recorrido: print TDA Board


;----------------------------------------------------------------------------------------;
;------------------------------ Funcion game-get-board ------------------------------;