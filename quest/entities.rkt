#lang racket/base

;;; Base structs for the Quest language

(provide
  (struct-out pos)
  (struct-out rect)
  (struct-out sprite)
  (struct-out zone)
  (struct-out spawner)
  (struct-out scrolling-bg)
  dimension%)

(require
  racket/class
  lux/chaos/gui/key)

(struct pos (x y) #:mutable)
(struct rect pos (w h) #:mutable)
(struct sprite (name path hitbox))
(struct zone (name title rect objects))
(struct spawner (rect objects))
(struct scrolling-bg (sprite direction speed))

(define dimension%
  (class object%
    (init-field title sprites zones)

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
