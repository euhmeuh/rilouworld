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
  "database.rkt"
  "../utils/struct.rkt"
  "../utils/anaphora.rkt"
  (prefix-in quest: "../quest/entities.rkt"))

(define *nb-of-layers* 8)
(define *fps* 60.0)
(define current-frame (make-parameter 0))

(struct loop (options dimension renderer)
  #:methods gen:word
  [(define (word-fps self) *fps*)
   (define (word-label self frame-time)
     (lux-standard-label (engine-options-title (loop-options self)) frame-time))
   (define (word-output self)
     ((loop-renderer self)))
   (define (word-event self event)
     (send (loop-dimension self) handle-event self event))
   (define (word-tick self)
     (send (loop-dimension self) emit 'tick)
     (current-frame (add1 (current-frame)))
     self)])

(struct engine-options (width height title))

(define (make-render-context width height sprite-db)
  (gl:stage-draw/dc sprite-db width height *nb-of-layers*))

(define (make-layer-config options)
  (with-values options (width height) from engine-options
    (vector (layer (/ width 2.) (/ height 2.)))))

(define (make-renderer options render-context sprite-db dimension)
  (define static-states
    (thunk
      (map (curryr render-static-sprite sprite-db)
           (send dimension collect-static-sprites))))
  (define dynamic-states
    (thunk
      (map (curryr render-sprite sprite-db)
           (send dimension collect-dynamic-sprites))))
  (thunk
    (render-context (make-layer-config options)
                    (static-states)
                    (dynamic-states))))

(define (render-sprite the-sprite sprite-db)
  (with-values the-sprite (pos image layer) from quest:sprite
    (sprite (quest:pos-x pos) (quest:pos-y pos)
            (sprite-idx sprite-db (get-and-update-sprite! image))
            #:layer layer
            #:pal-idx (palette-idx sprite-db 'palette))))

(define (render-static-sprite sprite sprite-db)
  #f)

(define (get-and-update-sprite! image)
  (cond
    [(quest:animation? image)
     (get-and-update-animation! image)]
    [else
     (quest:resource-name image)]))

(define (get-and-update-animation! animation)
  (with-values animation (frames length last-change frame) from quest:animation
    (define now (current-frame))
    (when (>= (- now last-change)
              (list-ref frames frame))
      (set! frame (remainder (add1 frame) length))
      (quest:set-animation-frame! animation frame)
      (quest:set-animation-last-change! animation now))
    (sprite-ref (quest:resource-name animation) frame)))

(define (make-loop options dimension account)
  (define sprite-db
    (make-database (get-field resources dimension)))
  (define render-context
    (with-values options (width height) from engine-options
      (make-render-context width height sprite-db)))
  (define renderer
    (make-renderer options render-context sprite-db dimension))
  (loop options dimension renderer))

(define (engine-start options dimension account)
  (call-with-chaos
    (make-gui #:mode gl:gui-mode)
    (thunk
      (fiat-lux (make-loop options dimension account)))))
