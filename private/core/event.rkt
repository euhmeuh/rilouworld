#lang racket/base

(provide
  (struct-out event)
  event-is?)

(struct event (type value))

(define (event-is? type event)
  (eq? type (event-type event)))
