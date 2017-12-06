#lang racket/base

;;; game logic for the nyancat dimension

(provide
  (rename-out (make-player player)))

(require
  racket/match
  "../quest/entities.rkt")

(struct player (sprite pos)
  #:methods gen:entity
  [(define (entity-emit self event)
     (when (key-event? event)
       (match (key-event-code event)
         ['left (displayln "Going left!")]
         ['right (displayln "Going right!")]
         ['up (displayln "Going up!")]
         ['down (displayln "Going down!")]
         [_ #t])))])

(define (make-player sprite [pos (pos 0 0)])
  (player sprite pos))
