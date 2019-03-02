#lang rilouworld/bundle

(define (on-door-touch self actor) (void))
(define (on-door-leave self actor) (void))
(define (on-door-activate self actor) (void))

(define-quest-actor door
  (attributes
    (model string?)
    (target symbol?))
  (events
    (touch on-door-touch)
    (leave on-door-leave)
    (activate on-door-activate)))

(define-quest-actor home
  (attributes
    (model string?)
    (doors (listof door?)))
  (events))
