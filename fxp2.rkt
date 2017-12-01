#lang racket/base

(require
  "engine.rkt"
  "state-machine.rkt")

(define-syntax-rule (dimension body ...)
  (begin
    (display "Hello world!")
    (newline)))

(dimension "Manafia"
  (zone 'golfia "Golfia" (rect 0 0 256 128)
    (resources
      (larva (monster))
      (home-out-001 (home-out)))
    (monster-nest 'larva (pos 50 60) 0.2)
    (home 'home-out-001 (pos 60 90) 'home-in-001))
  (zone-inside 'home-in-001 "Sample home" (size 100 80)
    (resources
      (sofa-001 (object "props/sofa-001.png" (hitbox (rect 0 20 30 20))))
      (lamp-001 (object "props/lamp-001.png"))
      (desk-001 (object "props/desk-001.png" (hitbox (rect 0 20 30 10)))))
    (props 'sofa-001 (pos 20 40))
    (props 'lamp-001 (pos 50 10))
    (props 'desk-001 (pos 70 40))))

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

(define player (make-player))

(define (update keys)
  (for-each 
    (lambda (key)
      (player 'action-from-key key))
    keys))

(display (format "Player state is:\n"))
(display (format "\tMovement: ~a\n" ((player 'movement-state) 'state)))
(display (format "\tAction: ~a\n" ((player 'action-state) 'state)))

(display (format "We send keyboard events!\n"))
(update '(up-pressed mouse-left-pressed))

(display (format "Player state is now:\n"))
(display (format "\tMovement: ~a\n" ((player 'movement-state) 'state)))
(display (format "\tAction: ~a\n" ((player 'action-state) 'state)))

(define options (options§ 512 384 "Rilouworld"))
(engine-start options)
