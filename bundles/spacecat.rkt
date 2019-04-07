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

(define (vec-lock-in-rect r v)
  (if r
      (vec (min
             (max (vec-x v) (rect-x r))
             (rect-w r))
           (min
             (max (vec-y v) (rect-y r))
             (rect-h r)))
      v))

(define (vec-apply func vectors #:start [start (vec .0 .0)])
  (for/fold ([x (vec-x start)]
             [y (vec-y start)]
             #:result (vec x y))
            ([val vectors])
    (let ([vec (if (number? val) (vec val val) val)])
      (values (func x (vec-x vec))
              (func y (vec-y vec))))))

(define (vec+ . vectors)
  (vec-apply + vectors))

(define (vec* . vectors)
  (vec-apply * vectors #:start (vec 1.0 1.0)))

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
    (vec-lock-in-rect
      (player-limits self)
      (vec+ (simple-sprite-pos self)
            (player-inertia self)))))

(define-quest-actor player (from simple-sprite)
  (attributes
    (limits rect? #:optional #f)
    (speed flonum? #:optional 1.0)
    (inertia vec? #:mutable #:private (vec .0 .0))
    (intention vec? #:mutable #:private (vec .0 .0)))
  (events
    (key on-player-key)
    (physics-tick on-player-physics-tick)))

(define-quest-actor troll (from simple-sprite))

(define-quest-actor star (from simple-sprite))
