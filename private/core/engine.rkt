#lang racket/base

;;; display the graphics on the screen
;;; and handle inputs

(provide
  ;; open a window and start the main game loop, handling inputs and displaying graphics
  engine-start
  ;; engine initial configuration
  (struct-out engine-options))

(require
  racket/function
  mode-lambda
  (prefix-in gl: mode-lambda/backend/gl)
  lux/chaos/gui
  lux/word
  rilouworld/private/core/database
  rilouworld/private/core/sprite
  rilouworld/private/utils/struct
  rilouworld/private/quest/world
  (prefix-in quest: rilouworld/quest))

(define LAYERS-NUM (length LAYERS))
(define FPS 60.0)

(define current-frame (make-parameter 0))

(struct loop (options world renderer)
  #:methods gen:word
  [(define (word-fps self) FPS)
   (define (word-label self frame-time)
     (lux-standard-label (engine-options-title (loop-options self)) frame-time))
   (define (word-output self)
     ((loop-renderer self)))
   (define (word-event self event)
     (handle-event (loop-world self) self event))
   (define (word-tick self)
     (current-frame (add1 (current-frame)))
     self)])

(struct engine-options (width height title))

(define (get-screen-size engine-options)
  (quest:size (engine-options-width engine-options)
              (engine-options-height engine-options)))

(define (make-render-context width height sprite-db)
  (gl:stage-draw/dc sprite-db width height LAYERS-NUM))

(define (make-layer-config options)
  (with-values options (width height) from engine-options
    (for/vector ([layer-name LAYERS])
      (layer (/ width 2.) (/ height 2.)))))

(define (make-renderer options render-context sprite-db world)
  (define static-states-cache #f)
  (define (static-states)
    (set! static-states-cache
          (or static-states-cache
              (map (curryr render-sprite sprite-db options)
                   (collect-static-sprites world))))
    static-states-cache)
  (define (dynamic-states)
    (map (curryr render-sprite sprite-db options)
         (collect-dynamic-sprites world)))
  (thunk
    (render-context (make-layer-config options)
                    (static-states)
                    (dynamic-states))))

(define (render-sprite the-sprite sprite-db engine-options)
  (with-values the-sprite (image layer) from sprite
    (define pos (sprite-pos the-sprite (get-screen-size engine-options)))
    (sprite (quest:pos-x pos) (quest:pos-y pos)
            (sprite-idx sprite-db (get-and-update-sprite! image))
            #:layer layer
            #:pal-idx (palette-idx sprite-db 'palette))))

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

(define (make-loop options world account)
  (initialize-world! world)
  (define sprite-db
    (make-database (world-resources world)))
  (define render-context
    (with-values options (width height) from engine-options
      (make-render-context width height sprite-db)))
  (define renderer
    (make-renderer options render-context sprite-db world))
  (loop options world renderer))

(define (engine-start options world account)
  (call-with-chaos
    (make-gui #:mode gl:gui-mode)
    (thunk
      (fiat-lux (make-loop options world account)))))
