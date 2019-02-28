#lang racket/base

;;; Base structs for the Quest language

(provide
  ;; layer definition
  LAYERS
  layer-idx
  ;; interface that allows the actor to react to events
  gen:receiver
  receiver?
  receiver-emit
  ;; utility procedure that emits an event to all given actors
  emit-to-all
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
  ;; utility recursive function that searches for sprites and sprites-holders
  ;; and returns a flatten list of sprites
  collect-sprites
  ;; base structs used in quest files
  (struct-out pos)
  (struct-out size)
  (struct-out rect)
  (struct-out resource)
  (struct-out image)
  (struct-out animation)
  (struct-out zone)
  (struct-out spawner)
  (struct-out spawn-info)
  (struct-out simple-sprite)
  (struct-out scrolling-bg)
  (struct-out particle)
  ;; base actor that holds a full world
  world%
  world?
  ;; give access to keyboard handling procedures
  (all-from-out lux/chaos/gui/key))

(require
  racket/generic
  racket/class
  racket/function
  racket/contract
  racket/list
  anaphoric
  lux/chaos/gui/key
  rilouworld/private/utils/struct)

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

(struct pos (x y) #:mutable)
(struct size (w h) #:mutable)
(struct rect pos (w h) #:mutable)
(struct resource (name path))
(struct image resource (hitbox))
(struct animation image (size frames length [frame #:mutable] [last-change #:mutable]))

(struct simple-sprite ([image #:mutable] pos)
  #:methods gen:sprite
  [(define (sprite-pos self screen-size)
     (simple-sprite-pos self))
   (define (sprite-image self)
     (simple-sprite-image self))
   (define (set-sprite-image! self image)
     (set-simple-sprite-image! self image))])

(struct spawner (rect spawn-infos)
  #:methods gen:receiver [])

(struct spawn-info (freq constructor args))

(struct scrolling-bg ([image #:mutable] direction speed)
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

(struct particle simple-sprite (direction lifetime))

(struct zone (name title size actors)
  #:methods gen:receiver
  [(define (receiver-emit self event)
     (emit-to-all event (zone-actors self)))]
  #:methods gen:sprite-holder
  [(define (sprite-holder-children self)
     (collect-sprites '() (zone-actors self)))])

(define world%
  (class object%
    (init-field title resources zones)
    (field [current-zone (car zones)]
           [current-sprites (initialize-sprites! current-zone resources)])

    (define/public (emit event)
      (receiver-emit current-zone event)
      #t)

    (define/public (handle-event game-loop event)
      (cond
       [(eq? event 'close) #f]
       [(emit event) game-loop]
       [else #f]))

    (define/public (collect-static-sprites)
      (filter sprite-static? current-sprites))

    (define/public (collect-dynamic-sprites)
      (filter (not/c sprite-static?) current-sprites))

    (super-new)))

(define world? (is-a?/c world%))

(define (initialize-sprites! zone resources)
  (define sprites (sprite-holder-children zone))
  (for-each
    (lambda (the-sprite)
      (with-values the-sprite (image) from sprite
        (when (symbol? image)
          (aif (get-resource-by-name image resources)
               (set-sprite-image! the-sprite it)
               (error 'unknown-image
                      (format "Could not find any image with the name '~a'." image))))))
    sprites)
  sprites)

(define (get-resource-by-name name resources)
  (findf (lambda (resource)
           (eq? name (resource-name resource)))
         resources))