#lang racket/base

(provide (except-out (all-from-out racket/base)
                     #%module-begin)
         (rename-out (module-begin #%module-begin))
         (rename-out (make-pos pos))
         (rename-out (make-rect rect))
         (rename-out (make-scrolling-bg scrolling-bg))
         (rename-out (make-player player))
         (rename-out (make-sprites sprites))
         (rename-out (make-zone zone))
         (rename-out (make-spawner spawner))
         (rename-out (make-dimension dimension))
         )

(require
  racket/class
  lux/chaos/gui/key
  "entities.rkt")

(define-syntax-rule (module-begin expr)
  (#%module-begin
    (provide dimension)
    (define dimension expr)))

(define (make-pos x y)
  (pos x y))

(define (make-rect x y w h)
  (rect x y w h))

(define (make-scrolling-bg sprite direction speed)
  (scrolling-bg sprite direction speed))

(define (make-player sprite [pos (pos 0 0)])
  (player sprite pos))

(define (make-dimension name sprites . zones)
  (make-object dimension% 'name sprites zones))

(define-syntax-rule (make-sprites (name path body ...) ...)
  (list (make-sprite 'name path body ...) ...))

(define (make-sprite name path [hitbox (rect 0 0 0 0)])
  (sprite name path hitbox))

(define (make-zone name title rect . objects)
  (zone name title rect objects))

(define (make-spawner rect . objects)
  (spawner rect objects))

(define dimension%
  (class object%
    (init-field name sprites zones)

    (define/public (emit event)
      (when (key-event? event)
        (displayln (format "Key ~s" (key-event-code event))))
      #t)

    (define/public (handle-event game-loop event)
      (cond
       [(eq? event 'close) #f]
       [(emit event) game-loop]
       [else #f]))

    (super-new)))

