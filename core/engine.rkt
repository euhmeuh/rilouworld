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
  "../utils/log.rkt"
  "../utils/struct.rkt"
  (prefix-in quest: "../quest/entities.rkt"))

(define *nb-of-layers* 8)
(define *fps* 60.0)

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
     self)])

(struct engine-options (width height title))

(define (make-database resources)
  (define sprite-db (make-sprite-db))
  (add-palette!/file sprite-db 'palette (build-path "." "dimensions" "palette.png"))
  (for-each
    (lambda (resource)
      (with-values resource (name path) from quest:resource
        (set! path (build-path "." "dimensions" path))
        (if (file-exists? path)
          (add-sprite!/file sprite-db
                            name
                            path
                            #:palette 'palette)
          (warning 'sprite-build
                   "Could not find file '~a' for resource '~a'." path name))))
    resources)
  (compile-sprite-db sprite-db))

(define (make-render-context width height sprite-db)
  (gl:stage-draw/dc sprite-db width height *nb-of-layers*))

(define (make-layer-config options)
  (with-values options (width height) from engine-options
    (vector (layer (/ width 2.) (/ height 2.)))))

(define (make-renderer options render-context sprite-db dimension)
  (define static-states
    (thunk
      (map (lambda (quest-sprite) #f)
             (send dimension collect-static-sprites))))
  (define dynamic-states
    (thunk
      (map (lambda (quest-sprite)
             (define pos (quest:sprite-pos quest-sprite))
             (sprite (quest:pos-x pos) (quest:pos-y pos)
                     (sprite-idx sprite-db (quest:sprite-resource quest-sprite))
                     #:layer (quest:sprite-layer quest-sprite)
                     #:pal-idx (palette-idx sprite-db 'palette)))
           (send dimension collect-dynamic-sprites))))
  (thunk
    (render-context (make-layer-config options)
                    (static-states)
                    (dynamic-states))))

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
