#lang racket/base

(provide
  ;; interface that allows the actor to react to events
  gen:receiver
  receiver?
  receiver-emit

  qualify-event
  emit-to-all)

(require
  racket/function
  racket/generic
  (only-in lux/chaos/gui/key
    key-event?
    key-event-code))

(define-generics receiver
  (receiver-emit receiver event)
  #:fallbacks
  [(define (receiver-emit receiver event)
     #t)])

(define (qualify-event event)
  (cond
    [(key-event? event) (values 'key (key-event-code event))]
    [else (values #f event)]))

(define (emit-to-all event actors)
  (for-each (curryr receiver-emit event)
            (filter receiver? actors)))
