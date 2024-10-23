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
;------------------------------ Funcion game-is-draw ------------------------------;

(define (game-is-draw game)
  (if (and (eq? (getRemainingPiecesPlayer (getGamePlayer1 game)) 0) (eq? (getRemainingPiecesPlayer (getGamePlayer2 game)) 0))
      #t
      (if (board-can-play (game-get-board game))
          #f
          #t
          )
      )
  )
;----------------------------------------------------------------------------------------;
;------------------------------ Funcion player-update-stas ------------------------------;

(define (actualizar-wins player)
  (define (loop player cont)
    (if (eq? cont 4)
        (cons (+ (car player) 1) (cdr player))
        (cons (car player) (loop (cdr player) (+ cont 1)))
        )
    )
  (loop player 1)
  )

(define (actualizar-losses player)
  (define (loop player cont)
    (if (eq? cont 5)
        (cons (+ (car player) 1) (cdr player))
        (cons (car player) (loop (cdr player) (+ cont 1)))
        )
    )
  (loop player 1)
  )

(define (actualizar-draws player)
 (define (loop player cont)
    (if (eq? cont 6)
        (cons (+ (car player) 1) (cdr player))
        (cons (car player) (loop (cdr player) (+ cont 1)))
        )
    )
  (loop player 1)
  )

(define (player-update-stats player result)
  (if (eq? result "win")
      (actualizar-wins player)
      (if (eq? result "loss")
          (actualizar-losses player)
          (actualizar-draws player)
          )
      )
  )

;----------------------------------------------------------------------------------------;
;------------------------------ Funcion game-get-current-player ------------------------------;


(define (game-get-current-player game)                                                             ;Funcion que entrega el jugador del turno actual
  (if (eq? (getIdPlayer (getGamePlayer1 game))(getGameCurrentTurn game))                           ;Dominio TDA Game
      (getGamePlayer1 game)                                                                        ;Recorrido TDA Player
      (getGamePlayer2 game) ;Supuniendo que el currentTurn se identifica por el id del player      ;Recursion no aplica
      )                     ;solo deberian existir estos 2 casos
  )



;----------------------------------------------------------------------------------------;
;------------------------------ Funcion game-get-board ------------------------------;

(define (game-get-board game)
  (display (game-get-board game))
  )


;----------------------------------------------------------------------------------------;
;------------------------------ Funcion game-get-board ------------------------------;


