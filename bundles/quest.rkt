#lang rilouworld/bundle

(require racket/contract)

(define direction? (or/c 'up 'down 'left 'right))

;; Base actors

(define-quest-actor simple-sprite
  (attributes
    (image image?)
    (pos pos? #:replace))
  (events))

(define-quest-actor scrolling-bg
  (attributes
    (image image?)
    (direction direction?)
    (speed flonum?))
  (events))

(define-quest-actor spawner
  (attributes
    (rect rect? #:replace)
    (actors (listof (list/c (list/c (or/c 'freq) flonum?) actor?))))
  (events))

(define-quest-actor particle
  (attributes
    (image image?)
    (pos pos? #:replace)
    (direction direction?)
    (speed flonum?))
  (events))
