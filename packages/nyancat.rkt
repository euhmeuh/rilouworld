#lang racket/base

;;; game logic for the nyancat dimension

(provide
  (rename-out (make-player player)
              (make-troll troll)
              (make-star star)
              ))

(require
  racket/match
  "../quest/entities.rkt")

(struct player simple-sprite ()
  #:methods gen:receiver
  [(define (receiver-emit self event)
     (define pos (simple-sprite-pos self))
     (define-values (x y) (values (pos-x pos) (pos-y pos)))
     (define speed 10.)
     (when (key-event? event)
       (match (key-event-code event)
         ['left (set-pos-x! pos (- x speed))]
         ['right (set-pos-x! pos (+ x speed))]
         ['up (set-pos-y! pos (- y speed))]
         ['down (set-pos-y! pos (+ y speed))]
         [_ #t])))])

(define (make-player resource-name [pos (pos 0 0)])
  (player resource-name pos))

(struct troll simple-sprite ())

(define (make-troll resource-name pos)
  (troll resource-name pos))

(struct star simple-sprite ())

(define (make-star resource-name pos)
  (star resource-name pos))
