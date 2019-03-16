#lang rilouworld/bundle

;;; game logic for the spacecat world

;; TODO: remove provide and require calls
(provide
  (rename-out [parse-simple-sprite simple-sprite]
              [parse-scrolling-bg scrolling-bg]
              [parse-spawner spawner]
              [parse-particle particle]
              [parse-player player]
              [parse-troll troll]
              [parse-star star]))

(require
  racket/match
  rilouworld/quest
  rilouworld/bundles/quest
  (for-syntax
    racket/base
    syntax/parse
    rilouworld/private/quest/props-meta))

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

(define-syntax (parse-player stx)
  (syntax-parse stx
    [(_ (~alt (~once (image <image>))
              (~once <pos>:pos-exp)) ...)
     #'(player <image> <pos>)]))

(define-quest-actor troll (from simple-sprite))

(define-syntax (parse-troll stx)
  (syntax-parse stx
    [(_ (~alt (~once (image <image>))
              (~once <pos>:pos-exp)) ...)
     #'(troll <image> <pos>)]))

(define-quest-actor star (from simple-sprite))

(define-syntax (parse-star stx)
  (syntax-parse stx
    [(_ (~alt (~once (image <image>))
              (~once <pos>:pos-exp)) ...)
     #'(star <image> <pos>)]))
