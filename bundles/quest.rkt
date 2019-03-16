#lang rilouworld/bundle

(require
  racket/contract
  rilouworld/quest
  rilouworld/private/core/receiver
  rilouworld/private/core/sprite
  (for-syntax
    racket/base
    syntax/parse
    rilouworld/private/quest/props-meta))

(provide parse-simple-sprite
         parse-scrolling-bg
         parse-spawner
         parse-particle
         (struct-out simple-sprite)
         (struct-out scrolling-bg)
         (struct-out spawner)
         (struct-out particle))

(define direction? (or/c 'up 'down 'left 'right))

;; Base actors

(define-quest-actor simple-sprite
  (attributes
    (image image? #:mutable)
    (pos pos?))
  (implements gen:sprite
    (define (sprite-pos self screen-size)
      (simple-sprite-pos self))
    (define (sprite-image self)
      (simple-sprite-image self))
    (define (set-sprite-image! self image)
      (set-simple-sprite-image! self image))))

(define-syntax (parse-simple-sprite stx)
  (syntax-parse stx
    #:datum-literals (image)
    [(_ (~alt (~once (image <image>))
              (~once <pos>:pos-exp)
              ) ...)
     #'(simple-sprite <image> <pos>)]))

(define-quest-actor scrolling-bg
  (attributes
    (image image? #:mutable)
    (direction direction?)
    (speed flonum?))
  (implements gen:receiver)
  (implements gen:sprite
    (define (sprite-static? self) #t)
    (define (sprite-pos self screen-size)
      (pos (/ (size-w screen-size) 2.)
           (/ (size-h screen-size) 2.)))
    (define (sprite-layer self)
      (layer-idx 'back))
    (define (sprite-image self)
      (scrolling-bg-image self))
    (define (set-sprite-image! self image)
      (set-scrolling-bg-image! self image))))

(define-syntax (parse-scrolling-bg stx)
  (syntax-parse stx
    #:datum-literals (image direction speed)
    [(_ (~alt (~once (image <image>))
              (~once (direction <direction>))
              (~once (speed <speed>))
              ) ...)
     #'(scrolling-bg <image> <direction> <speed>)]))

(define-quest-actor spawner
  (attributes
    (rect rect?)
    (actors spawn-info? #:list))
  (implements gen:receiver))

(struct spawn-info (freq constructor))

(define-syntax (parse-spawner stx)
  (syntax-parse stx
    #:literals (rect)
    #:datum-literals (actors freq)
    [(_ (~alt (~once <rect>:rect-exp)
              (~once (actors ((freq <freq>) <actor>) ...))
              ) ...)
     #'(spawner <rect>
                (list (spawn-info <freq> (lambda (x y) '<actor>)) ...))]))

(define-quest-actor particle (from simple-sprite)
  (attributes
    (direction direction?)
    (speed flonum?)
    (lifetime flonum?)))

(define-syntax (parse-particle stx)
  (syntax-parse stx
    #:datum-literals (image direction speed lifetime)
    [(_ (~alt (~once (image <image>))
              (~once <pos>:pos-exp)
              (~once (direction <direction>))
              (~once (speed <speed>))
              (~once (lifetime <lifetime>))
              ) ...)
     #'(particle <image> <pos> <direction> <speed> <lifetime>)]))
