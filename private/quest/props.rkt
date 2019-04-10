#lang racket/base

;;; Base structs for the Quest language

(provide
  ;; base structs used in quest files
  (struct-out vec)
  (struct-out size)
  (struct-out rect)
  rect-horizontal-range
  rect-vertical-range

  (struct-out change)
  (struct-out author)

  (struct-out resource)
  (struct-out image)
  (struct-out animation)

  (struct-out actor))

(struct vec (x y) #:mutable #:transparent)
(struct size (w h) #:mutable #:transparent)
(struct rect (x y w h) #:mutable #:transparent)

(struct change (version date desc) #:transparent)
(struct author (name section) #:transparent)

(struct resource (name path) #:transparent)
(struct image resource (hitbox) #:transparent)
(struct animation image (size frames length [frame #:mutable] [last-change #:mutable]) #:transparent)

(struct actor () #:transparent)

(define (rect-horizontal-range r)
  (list (rect-x r)
        (+ (rect-w r) (rect-x r))))

(define (rect-vertical-range r)
  (list (rect-y r)
        (+ (rect-h r) (rect-y r))))
