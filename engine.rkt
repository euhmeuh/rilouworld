#lang racket

(require
  mode-lambda
  mode-lambda/static
  lux
  lux/chaos/gui
  lux/chaos/gui/key
  (prefix-in gl: mode-lambda/backend/gl))

(define *label* "Rilouworld")
(define *width* 512)
(define *height* 384)

(define (prepare-render-context)
  (define gl-texture-layers 8)
  (define compiled-sprite-db (compile-sprite-db (make-sprite-db)))
  (gl:stage-draw/dc compiled-sprite-db
                    *width*
                    *height*
                    gl-texture-layers))

(define (prepare-layer-config)
  (vector))

(define (make-renderer render-context)
  (define static-states '())
  (define dynamic-states '())
  (define layer-config (prepare-layer-config))
  (lambda ()
    (render-context layer-config static-states dynamic-states)))

(struct game (render-context renderer)
  #:methods gen:word
  [(define (word-fps word) 60.0)
   (define (word-label word frame-time)
     (lux-standard-label *label* frame-time))
   (define (word-output word)
     ((game-renderer word)))
   (define (word-event word event)
     (cond
       [(or (eq? event 'close)
            (and (key-event? event)
                 (eq? 'escape (key-event-code event))))
        #f]
       [else word]))
   (define (word-tick word)
     word)])

(call-with-chaos
  (make-gui #:mode gl:gui-mode)
  (lambda ()
    (define render-context (prepare-render-context))
    (fiat-lux (game render-context
                    (make-renderer render-context)))))
