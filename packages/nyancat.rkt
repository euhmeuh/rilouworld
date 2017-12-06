#lang racket/base

;;; game logic for the nyancat dimension

(provide
  (rename-out (make-player player)))

(require
  racket/match
  "../quest/entities.rkt")

(struct player (resource pos)
  #:methods gen:receiver
  [(define (receiver-emit self event)
     (define pos (player-pos self))
     (define-values (x y) (values (pos-x pos) (pos-y pos)))
     (define speed 10.)
     (when (key-event? event)
       (match (key-event-code event)
         ['left (set-pos-x! pos (- x speed))]
         ['right (set-pos-x! pos (+ x speed))]
         ['up (set-pos-y! pos (- y speed))]
         ['down (set-pos-y! pos (+ y speed))]
         [_ #t])))]
  #:methods gen:sprite
  [(define (sprite-pos self) (player-pos self))
   (define (sprite-resource self) (player-resource self))])

(define (make-player resource [pos (pos 0 0)])
  (player resource pos))
