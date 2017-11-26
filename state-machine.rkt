#lang racket

(require "anaphora.rkt")

(provide make-transitions make-state)

(define (make-transitions start . rules)
  (define (try current-state event)
    (define (get-new-state rule)
      (caddar rule))
    (define (from-this-state? rule)
      (eq? (caar rule) current-state))
    (define (handles-this-event? rule)
      (memq event (cadr rule)))

    (define found-rule
      (findf handles-this-event?
             (filter from-this-state? rules)))
    (if found-rule
      (get-new-state found-rule)
      #f))

  (define (dispatch m . args)
    (cond
      ((eq? m 'start) start)
      ((eq? m 'try) (apply try args))))
  dispatch)

(define (make-state transitions)
  (let ((subscriptions (make-hash))
        (state (transitions 'start)))

    (define (execute event)
      (awhen (hash-ref subscriptions event #f)
        (apply it event)))

    (define (emit event)
      (awhen (transitions 'try state event)
        (set! state it)
        (execute event)))

    (define (subscribe event function)
      (hash-set! subscriptions event function))

    (define (dispatch m . args)
      (cond
        ((eq? m 'state) state)
        ((eq? m 'emit) (apply emit args))
        ((eq? m 'subscribe) (apply subscribe args))))
    dispatch))