#lang rilouworld/bundle

(require
  rilouworld/bundles/quest
  rilouworld/private/core/sprite)

(define (on-door-touch self actor) (void))
(define (on-door-leave self actor) (void))
(define (on-door-activate self actor) (void))

(define-quest-actor door (from simple-sprite)
  (attributes
    (model string?)
    (target symbol?))
  (events
    (touch on-door-touch)
    (leave on-door-leave)
    (activate on-door-activate)))

(define-quest-actor home (from simple-sprite)
  (attributes
    (model string?)
    (doors door? #:list)))
