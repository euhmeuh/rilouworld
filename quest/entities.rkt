#lang racket/base

;;; Base structs for the Quest language

(provide
  (struct-out pos)
  (struct-out rect)
  (struct-out sprite)
  (struct-out zone)
  (struct-out spawner)
  (struct-out scrolling-bg)
  (struct-out player))

(struct pos (x y) #:mutable)
(struct rect pos (w h) #:mutable)
(struct sprite (name path hitbox))
(struct zone (name title rect objects))
(struct spawner (rect objects))
(struct scrolling-bg (sprite direction speed))
(struct player (sprite pos))
