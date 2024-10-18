#lang racket
(require "TDA-Piece.rkt")
(require "TDA-Player.rkt")
(require "TDA-Board.rkt")

                                                                ;Funcion constructora del TDA Game
(define (game player1 player2 board current-turn)               ;Dominio: 2 TDA-Player, 1 TDA-Board y 1 int
  (list player1 player2 board current-turn)                     ;Recorrido: 1 lista que contiene los elementos de entrada
  )                                                             ;No aplica recursion



;--------------------------------------------------------------------;
;------------------------- Funciones Get ----------------------------;

(define (getGamePlayer1 game)
  (car game)
  )

(define (getGamePlayer2 game)
  (car (cdr Game))
  )

(define (game-get-board game)
  (car (cdr (cdr game)))
  )

(define (getGameCurrentTurn game)
  (car (cdr (cdr (cdr game))))
  )

;----------------------------------------------------------------------------------;
;------------------------- Funcion game-player-set-move ---------------------------;








