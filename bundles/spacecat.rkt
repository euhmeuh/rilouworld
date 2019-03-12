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
  rilouworld/private/core/receiver
  (only-in lux/chaos/gui/key
           key-event?
           key-event-code)
  (for-syntax
    racket/base
    syntax/parse
    rilouworld/private/quest/props-meta))

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

(define-quest-actor player
  (attributes)
  (events))

(define-syntax (parse-player stx)
  (syntax-parse stx
    [(_ (~alt (~once (image <image>))
              (~once <pos>:pos-exp)) ...)
     #'(player <image> <pos>)]))

(struct troll simple-sprite ())

(define-syntax (parse-troll stx)
  (syntax-parse stx
    [(_ (~alt (~once (image <image>))
              (~once <pos>:pos-exp)) ...)
     #'(troll <image> <pos>)]))

(struct star simple-sprite ())

(define-syntax (parse-star stx)
  (syntax-parse stx
    [(_ (~alt (~once (image <image>))
              (~once <pos>:pos-exp)) ...)
     #'(star <image> <pos>)]))
