#lang racket/base

;;; Base structs for the Quest language

(provide
  gen:entity
  entity?
  entity-emit
  (struct-out pos)
  (struct-out rect)
  (struct-out sprite)
  (struct-out zone)
  (struct-out spawner)
  (struct-out scrolling-bg)
  dimension%
  (all-from-out lux/chaos/gui/key))

(require
  racket/generic
  racket/class
  racket/function
  lux/chaos/gui/key)

(define-generics entity
  (entity-emit entity event)
  #:fallbacks
  [(define (entity-emit entity event)
     #t)])

(struct pos (x y) #:mutable)
(struct rect pos (w h) #:mutable)
(struct sprite (name path hitbox))

(struct spawner (rect objects)
  #:methods gen:entity [])

(struct scrolling-bg (sprite direction speed)
  #:methods gen:entity [])

(struct zone (name title rect objects)
  #:methods gen:entity
  [(define/generic super-emit entity-emit)
   (define (entity-emit self event)
     (for-each (curryr super-emit event) (zone-objects self)))])

(define dimension%
  (class object%
    (init-field title sprites zones)
    (field [current-zone (car zones)])

    (define/public (emit event)
      (entity-emit current-zone event)
      #t)

    (define/public (handle-event game-loop event)
      (cond
       [(eq? event 'close) #f]
       [(emit event) game-loop]
       [else #f]))

    (super-new)))
