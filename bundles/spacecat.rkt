#lang rilouworld/bundle

;;; game logic for the spacecat world

(provide
  (actor-out player)
  (actor-out troll)
  (actor-out star))

(require
  rilouworld/bundles/quest)

(define (keys->intention keys speed)
  (let* ([x (if (memq 'right keys) speed 0)]
         [x (if (memq 'left keys) (- x speed) x)]
         [y (if (memq 'down keys) speed 0)]
         [y (if (memq 'up keys) (- y speed) y)])
    (vec x y)))

(define (vec-apply func vectors [start (cons 0 0)])
  (for/fold ([x (car start)]
             [y (cdr start)]
             #:result (vec x y))
            ([val vectors])
    (let ([vec (if (number? val) (vec val val) val)])
      (values (func x (vec-x vec))
              (func y (vec-y vec))))))

(define (vec+ . vectors)
  (vec-apply + vectors))

(define (vec* . vectors)
  (vec-apply * vectors (cons 1 1)))

(define (on-player-key self key keys)
  (set-player-intention! self
    (keys->intention keys (player-speed self))))

(define fake-friction 0.9)

(define (on-player-physics-tick self delta)
  (set-player-inertia! self
    (vec* (vec+ (player-inertia self)
                (player-intention self))
          fake-friction))
  (set-simple-sprite-pos! self
    (vec+ (simple-sprite-pos self)
          (player-inertia self))))

(define-quest-actor player (from simple-sprite)
  (attributes
    (speed flonum? #:mutable #:default 1.0)
    (inertia vec? #:mutable #:private)
    (intention vec? #:mutable #:private))
  (events
    (key on-player-key)
    (physics-tick on-player-physics-tick)))

(define-quest-actor troll (from simple-sprite))

(define-quest-actor star (from simple-sprite))
