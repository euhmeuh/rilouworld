#lang racket/base

;;; display the graphics on the screen
;;; and handle inputs

(provide
  ;; open a window and start the main game loop, handling inputs and displaying graphics
  engine-start
  ;; engine initial configuration
  (struct-out engine-options))

(require
  racket/class
  racket/function
  mode-lambda
  mode-lambda/static
  lux
  lux/chaos/gui
  (prefix-in gl: mode-lambda/backend/gl)
  "struct-utils.rkt")

(define *nb-of-layers* 8)

(define (prepare-sprite-db)
  (define sprite-db (make-sprite-db))
  (add-palette!/file sprite-db 'palette (build-path "." "palette.png"))
  (add-sprite!/file sprite-db 'nyancat (build-path "." "nyancat.png") #:palette 'palette)
  (compile-sprite-db sprite-db))

(define (prepare-render-context width height sprite-db)
  (gl:stage-draw/dc sprite-db width height *nb-of-layers*))

(define (prepare-layer-config options)
  (with-values options (width height) from engine-options
    (vector (layer (/ width 2.) (/ height 2.)))))

(define (make-renderer options render-context sprite-db)
  (define static-states '())
  (define dynamic-states
    (thunk
      (list (sprite 50. 50.
                    (sprite-idx sprite-db 'nyancat)
                    #:layer 0
                    #:pal-idx (palette-idx sprite-db 'palette)))))
  (thunk
    (render-context (prepare-layer-config options) static-states (dynamic-states))))

(struct game (options dimension renderer)
  #:methods gen:word
  [(define (word-fps word) 60.0)
   (define (word-label word frame-time)
     (lux-standard-label (engine-options-title (game-options word)) frame-time))
   (define (word-output word)
     ((game-renderer word)))
   (define (word-event word event)
     (send (game-dimension word) handle-event word event))
   (define (word-tick word)
     word)])

(struct engine-options (width height title))

(define (engine-start options dimension account)
  (call-with-chaos
    (make-gui #:mode gl:gui-mode)
    (thunk
      (define sprite-db (prepare-sprite-db))
      (define render-context
        (with-values options (width height) from engine-options
          (prepare-render-context width height sprite-db)))
      (fiat-lux (game options
                      dimension
                      (make-renderer options render-context sprite-db))))))
