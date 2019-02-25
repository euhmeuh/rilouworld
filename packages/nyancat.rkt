#lang racket/base

;;; game logic for the nyancat world

(provide
  (rename-out (make-player player)
              (make-troll troll)
              (make-star star)
              ))

(require
  racket/match
  rilouworld/quest)

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

(define (make-player image [pos (pos 0 0)])
  (player image pos))

(struct troll simple-sprite ())

(define (make-troll image pos)
  (troll image pos))

(struct star simple-sprite ())

(define (make-star image pos)
  (star image pos))
