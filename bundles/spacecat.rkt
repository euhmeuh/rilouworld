#lang rilouworld/bundle

;;; game logic for the spacecat world

(provide
  (actor-out player)
  (actor-out troll)
  (actor-out star))

(require
  racket/match
  rilouworld/bundles/quest)

(define (on-player-key self key)
  (let* ([pos (simple-sprite-pos self)]
         [x (pos-x pos)]
         [y (pos-y pos)]
         [speed 10.])
    (match key
      ['left (set-pos-x! pos (- x speed))]
      ['right (set-pos-x! pos (+ x speed))]
      ['up (set-pos-y! pos (- y speed))]
      ['down (set-pos-y! pos (+ y speed))]
      [_ #t])))

(define-quest-actor player (from simple-sprite)
  (events
    (key on-player-key)))

(define-quest-actor troll (from simple-sprite))

(define-quest-actor star (from simple-sprite))
