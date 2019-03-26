#lang racket/base

(provide
  ;; interface that allows the actor to react to events
  gen:receiver
  receiver?
  receiver-emit
  emit-to-all)

(require
  racket/function
  racket/generic)

(define-generics receiver
  (receiver-emit receiver event)
  #:fallbacks
  [(define (receiver-emit receiver event)
     #t)])

(define (emit-to-all event actors)
  (for-each (curryr receiver-emit event)
            (filter receiver? actors)))
