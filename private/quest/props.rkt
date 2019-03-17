#lang racket/base

;;; Base structs for the Quest language

(provide
  ;; base structs used in quest files
  (struct-out pos)
  (struct-out size)
  (struct-out rect)

  (struct-out change)
  (struct-out author)

  (struct-out resource)
  (struct-out image)
  (struct-out animation)

  (struct-out actor))

(struct pos (x y) #:mutable #:transparent)
(struct size (w h) #:mutable #:transparent)
(struct rect pos (w h) #:mutable #:transparent)

(struct change (version date desc) #:transparent)
(struct author (name section) #:transparent)

(struct resource (name path) #:transparent)
(struct image resource (hitbox) #:transparent)
(struct animation image (size frames length [frame #:mutable] [last-change #:mutable]) #:transparent)

(struct actor () #:transparent)
