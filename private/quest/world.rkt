#lang racket/base

(provide
  ;; layer definition
  LAYERS
  layer-idx

  ;; interface that allows the actor to react to events
  gen:receiver
  receiver?
  receiver-emit

  ;; interface that allows the actor to display itself as a sprite
  gen:sprite
  sprite?
  sprite-pos
  sprite-image
  set-sprite-image!
  sprite-layer
  sprite-static?

  ;; interface for actors that have gen:sprite children
  gen:sprite-holder
  sprite-holder?
  sprite-holder-children

  ;; world functions
  handle-event
  initialize-world!
  collect-static-sprites
  collect-dynamic-sprites

  ;; base structs
  (struct-out world)
  (struct-out zone))

(require
  racket/contract/base
  racket/function
  racket/generic
  racket/list
  (only-in rilouworld/private/quest/props
           pos))

(define LAYERS '(back back-actors player front-actors front ui))
(define (layer-idx name) (index-of LAYERS name))

(define-generics receiver
  (receiver-emit receiver event)
  #:fallbacks
  [(define (receiver-emit receiver event)
     #t)])

(define (emit-to-all event actors)
  (for-each (curryr receiver-emit event)
            (filter receiver? actors)))

(define-generics sprite
  (sprite-pos sprite screen-size)
  (sprite-image sprite)
  (set-sprite-image! sprite image)
  (sprite-layer sprite)
  (sprite-static? sprite)
  #:fallbacks
  [(define (sprite-pos sprite screen-size)
     (pos 0. 0.))
   (define (sprite-image sprite)
     #f)
   (define (set-sprite-image! sprite image)
     (void))
   (define (sprite-layer sprite)
     (layer-idx 'back-actors))
   (define (sprite-static? sprite)
     #f)])

(define-generics sprite-holder
  (sprite-holder-children sprite-holder))

;; recursively find sprites in a list of actors
(define (collect-sprites result actors)
  (cond
    [(empty? actors) result]
    [(sprite-holder? (car actors))
     (collect-sprites (cons (sprite-holder-children (car actors))
                            result)
                      (cdr actors))]
    [(sprite? (car actors))
     (collect-sprites (cons (car actors) result)
                      (cdr actors))]
    [else
     (collect-sprites result (cdr actors))]))

(struct zone (id name map type rectangles actors)
  #:methods gen:receiver
  [(define (receiver-emit self event)
     (emit-to-all event (zone-actors self)))]
  #:methods gen:sprite-holder
  [(define (sprite-holder-children self)
     (collect-sprites '() (zone-actors self)))])

(struct world (name uuid version desc changelog authors resources zones
               [current-zone #:mutable]
               [current-sprites #:mutable]))

(define (handle-event world game-loop event)
  (cond
   [(eq? event 'close) #f]
   [(receiver-emit (world-current-zone world) event) game-loop]
   [else #f]))

(define (initialize-world! world)
  (set-world-current-zone! world (car (world-zones world)))
  (set-world-current-sprites!
    world
    (sprite-holder-children (world-current-zone world))))

(define (collect-static-sprites world)
  (filter sprite-static? (world-current-sprites world)))

(define (collect-dynamic-sprites world)
  (filter (not/c sprite-static?) (world-current-sprites world)))
