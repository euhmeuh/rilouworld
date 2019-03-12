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

(struct pos (x y) #:mutable)
(struct size (w h) #:mutable)
(struct rect pos (w h) #:mutable)

(struct change (version date desc))
(struct author (name section))

(struct resource (name path))
(struct image resource (hitbox))
(struct animation image (size frames length [frame #:mutable] [last-change #:mutable]))

(struct actor ())
