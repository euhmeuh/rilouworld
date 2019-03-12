#lang rilouworld/bundle

(require
  racket/contract
  rilouworld/quest
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
    #:datum-literals (image)
    [(_ (~alt (~once (image <image>))
              (~once <pos>:pos-exp)
              ) ...)
     #'(simple-sprite <image> <pos>)]))

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

(define-syntax (parse-scrolling-bg stx)
  (syntax-parse stx
    #:datum-literals (image direction speed)
    [(_ (~alt (~once (image <image>))
              (~once (direction <direction>))
              (~once (speed <speed>))
              ) ...)
     #'(scrolling-bg <image> <direction> <speed>)]))

(struct spawner actor (rect spawn-infos)
  #:methods gen:receiver [])

(struct spawn-info (freq constructor))

(define-quest-actor spawner
  (attributes
    (rect rect? #:replace)
    (actors (listof (list/c (list/c (or/c 'freq) flonum?) actor?))))
  (events))

(define-syntax (parse-spawner stx)
  (syntax-parse stx
    #:literals (rect)
    #:datum-literals (actors freq)
    [(_ (~alt (~once <rect>:rect-exp)
              (~once (actors ((freq <freq>) <actor>) ...))
              ) ...)
     #'(spawner <rect>
                (list (spawn-info <freq> (lambda (x y) '<actor>)) ...))]))

(struct particle simple-sprite (direction speed lifetime))

(define-quest-actor particle
  (attributes
    (image image?)
    (pos pos? #:replace)
    (direction direction?)
    (speed flonum?)
    (lifetime flonum?))
  (events))

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
