#lang rilouworld/bundle

(require racket/contract
         syntax/parse
         (for-syntax racket/base syntax/parse))

(provide parse-simple-sprite
         (struct-out simple-sprite)
         (struct-out scrolling-bg)
         (struct-out spawner)
         (struct-out particle))

(define direction? (or/c 'up 'down 'left 'right))

;; Base actors

(struct simple-sprite actor ([image #:mutable] pos)
  #:methods gen:sprite
  [(define (sprite-pos self screen-size)
     (simple-sprite-pos self))
   (define (sprite-image self)
     (simple-sprite-image self))
   (define (set-sprite-image! self image)
     (set-simple-sprite-image! self image))])

(define-quest-actor simple-sprite
  (attributes
    (image image?)
    (pos pos? #:replace))
  (events))

(define-syntax (parse-simple-sprite stx)
  (syntax-parse stx
    #:literals (pos)
    #:datum-literals (image)
    [(_ (image img) (~and p (pos x y)))
     #'(simple-sprite img p)]))

(struct scrolling-bg actor ([image #:mutable] direction speed)
  #:methods gen:receiver []
  #:methods gen:sprite
  [(define (sprite-static? self) #t)
   (define (sprite-pos self screen-size)
     (pos (/ (size-w screen-size) 2.)
          (/ (size-h screen-size) 2.)))
   (define (sprite-layer self)
     (layer-idx 'back))
   (define (sprite-image self)
     (scrolling-bg-image self))
   (define (set-sprite-image! self image)
     (set-scrolling-bg-image! self image))])

(define-quest-actor scrolling-bg
  (attributes
    (image image?)
    (direction direction?)
    (speed flonum?))
  (events))

(struct spawner actor (rect spawn-infos)
  #:methods gen:receiver [])

(struct spawn-info (freq constructor args))

(define-quest-actor spawner
  (attributes
    (rect rect? #:replace)
    (actors (listof (list/c (list/c (or/c 'freq) flonum?) actor?))))
  (events))

(struct particle simple-sprite (direction lifetime))

(define-quest-actor particle
  (attributes
    (image image?)
    (pos pos? #:replace)
    (direction direction?)
    (speed flonum?))
  (events))
