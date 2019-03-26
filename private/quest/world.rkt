#lang racket/base

(provide
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
  rilouworld/private/core/event
  rilouworld/private/core/receiver
  rilouworld/private/core/sprite)

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
   [(event-is? 'close event) #f]
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
