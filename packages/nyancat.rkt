#lang racket/base

;;; game logic for the nyancat dimension

(provide
  (rename-out (make-player player)))

(require
  "../quest/entities.rkt")

(struct player (sprite pos))

(define (make-player sprite [pos (pos 0 0)])
  (player sprite pos))
