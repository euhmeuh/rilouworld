#lang racket/base

(provide
  ;; create a new player object
  make-player)

(require
  rilouworld/core/state-machine)

(define movement-transitions
  (make-transitions
    'idle
    '((idle -> moving) (left-pressed right-pressed))
    '((moving -> idle) (left-released right-released))
    '((idle -> jumping) (up-pressed))
    '((moving -> jumping) (up-pressed))
    '((jumping -> falling) (vertical-momentum-felt))
    '((falling -> idle) (grounded))))

(define action-transitions
  (make-transitions
    'idle
    '((idle -> attacking) (mouse-left-pressed mouse-right-pressed))
    '((attacking -> idle) (attack-finished))
    '((idle -> using) (num-1-pressed num-2-pressed num-3-pressed num-4-pressed))
    '((using -> idle) (use-finished))))

(define (make-body) 0)
(define (apply-force a b c) 0)
(define (use-left-weapon) 0)
(define (use-right-weapon) 0)
(define vector-left 0)
(define vector-right 0)

(define (make-player)
  (let ((movement-state (make-state movement-transitions))
        (action-state (make-state action-transitions))
        (body (make-body)))

    (define (move key)
      (cond
        ((eq? key 'left-pressed) (apply-force body vector-left))
        ((eq? key 'right-pressed) (apply-force body vector-right))))

    (define (attack key)
      (cond
        ((eq? key 'mouse-left-pressed) (use-left-weapon))
        ((eq? key 'mouse-right-pressed) (use-right-weapon))))

    (movement-state 'subscribe 'moving move)
    (action-state 'subscribe 'attacking attack)

    (define (action-from-key key)
      (movement-state 'emit key)
      (action-state 'emit key))

    (define (dispatch m . args)
      (cond
        ((eq? m 'movement-state) movement-state)
        ((eq? m 'action-state) action-state)
        ((eq? m 'action-from-key) (apply action-from-key args))))
    dispatch))
