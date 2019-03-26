#lang rilouworld/bundle

(provide
  (actor-out simple-sprite)
  (actor-out scrolling-bg)
  (actor-out spawner)
  (actor-out particle)

  (struct-out freq))

(require
  rilouworld/private/core/sprite
  racket/contract)

(define direction? (or/c 'up 'down 'left 'right))

;; Base actors

(define-quest-actor simple-sprite
  (attributes
    (image image? #:mutable)
    (pos vec? #:mutable))
  (implements gen:sprite
    (define (sprite-pos self screen-size)
      (simple-sprite-pos self))
    (define (sprite-image self)
      (simple-sprite-image self))
    (define (set-sprite-image! self image)
      (set-simple-sprite-image! self image))))

(define-quest-actor scrolling-bg
  (attributes
    (image image? #:mutable)
    (direction direction?)
    (speed flonum?))
  (events)
  (implements gen:sprite
    (define (sprite-static? self) #t)
    (define (sprite-pos self screen-size)
      (vec (/ (size-w screen-size) 2.)
           (/ (size-h screen-size) 2.)))
    (define (sprite-layer self)
      (layer-idx 'back))
    (define (sprite-image self)
      (scrolling-bg-image self))
    (define (set-sprite-image! self image)
      (set-scrolling-bg-image! self image))))

(define-quest-actor spawner
  (attributes
    (rect rect?)
    (actors freq? #:list))
  (events))

(struct freq (value constructor))

(define-quest-actor particle (from simple-sprite)
  (attributes
    (direction direction?)
    (speed flonum?)
    (lifetime flonum?)))
