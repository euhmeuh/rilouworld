#lang racket/base

(provide
  ;; layer definition
  LAYERS
  layer-idx

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

  collect-sprites)

(require
  racket/generic
  racket/list
  (only-in rilouworld/private/quest/props vec))

(define LAYERS '(back back-actors player front-actors front ui))
(define (layer-idx name) (index-of LAYERS name))

(define-generics sprite
  (sprite-pos sprite screen-size)
  (sprite-image sprite)
  (set-sprite-image! sprite image)
  (sprite-layer sprite)
  (sprite-static? sprite)
  #:fallbacks
  [(define (sprite-pos sprite screen-size)
     (vec 0. 0.))
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
     (collect-sprites (append (sprite-holder-children (car actors))
                              result)
                      (cdr actors))]
    [(sprite? (car actors))
     (collect-sprites (cons (car actors) result)
                      (cdr actors))]
    [else
     (collect-sprites result (cdr actors))]))
